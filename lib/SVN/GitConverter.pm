package SVN::GitConverter;
use Moo;
use autodie ':all';
use Git::Wrapper;
use File::Basename qw(basename);
use File::Copy qw(copy);
use Cwd qw(realpath);
use JSON qw(encode_json decode_json);
use Carp;
use File::chdir;

sub svn_info {
  my $url = shift;

  my %info;
  open my $fh, '-|', 'svn', 'info', $url;
  while (my $line = <$fh>) {
    my ($item, $value) = split /:/, $line, 2;
    next unless $value;
    for ($item, $value) {
      s/\s+$//;
      s/^\s+//;
    }
    $info{$item} = $value;
  }

  return %info;
}

sub git_svn_id {
  my @commit_message = map { split /\n/, $_ } @_;
  my ($svn_id) = grep { /^git-svn-id:/ } reverse @commit_message;
  return
    unless $svn_id;
  $svn_id =~ s/^git-svn-id:\s+//;
  $svn_id =~ s/\s+(\S+)$//;
  my $uuid = $1;
  $svn_id =~ s/\@(\d+)$//;
  my $rev = $1;
  my $url = $svn_id;
  return wantarray ? ($url, $rev, $uuid) : $rev;
}

has repo => (
  is => 'ro',
  required => 1,
  coerce => sub {
    -d $_[0] ? realpath($_[0]) : File::Spec->rel2abs($_[0])
  },
);

has git => (
  is => 'lazy',
  default => sub { Git::Wrapper->new($_[0]->repo) },
);

sub init {
  my ($self, $file) = @_;
  my $repo = $self->repo;
  if (!-d $repo) {
    mkdir $repo;
  }
  my $git = $self->git;
  my %config;
  my @config = `git config --file=$file --get-regexp '^svn-converter\\.'`;
  chomp @config;
  my %multi = map { $_ => 1 } qw(branch-map tag-map extra-refs graft-revs branch-roots tag-roots);
  for my $config (@config) {
    my ($name, $value) = split /\s+/, $config, 2;
    my @section = split /\./, $name;
    shift @section;
    $name = $section[0];

    if ($multi{$name}) {
      push @{ $config{$name} ||= [] }, $value;
    }
    else {
      $config{$name} = $value;
    }
  }

  $git->init;

  my @locations;
  my $trunk = delete $config{trunk};
  push @locations, '--trunk', $trunk;
  for my $type (qw(branch tag)) {
    push @locations, map {; "--$type", $_ } @{ delete $config{"${type}-roots"} ||[] };
  }

  my $svn_url = delete $config{'svn-url'};
  if ($svn_url !~ m{^\w+://}) {
    for my $try ($svn_url, "mirrors/$svn_url") {
      my @parts = split m{/}, $try;
      for my $i (0..$#parts) {
        if (-d File::Spec->catdir(@parts[0..$i], 'db')) {
          $svn_url = "file://" . realpath($try);
          last;
        }
      }
    }
  }
  {
    local $ENV{PERL5LIB};
    local $ENV{PERL5OPT};
    $git->svn('init', '--prefix=svn/', @locations, '--rewrite-root', 'svn://localhost/', $svn_url);
  }
  $git->config('svn.preserve-empty-dirs', 'true');
  $git->config('svn.placeholder-filename', '.gitignore');

  $git->config({unset_all => 1}, 'svn-remote.svn.fetch');
  $git->config({add => 1}, 'svn-remote.svn.fetch', "$trunk:refs/remotes/svn/trunk");
  for my $map_type (qw(branch tag)) {
    if (my $map = delete $config{"$map_type-map"}) {
      for my $mapping (@$map) {
        my ($name, $location) = split /:/, $mapping, 2;
        $name = 'tags/'.$name
          if $map_type eq 'tag';
        $git->config({add => 1}, 'svn-remote.svn.fetch', "$location:refs/remotes/svn/$name");
      }
    }
  }

  my $authors_source = delete $config{authors};
  my $authors_file = $self->repo."/.git/svn-authors";
  if ($authors_source) {
    File::Copy::copy("authors/".$authors_source, $authors_file);
  }
  else {
    open my $fh, '>', $authors_file;
  }

  $git->config('svn.authorsfile', $authors_file);
  open my $fh, '>', "$repo/.git/author-generate";
  print { $fh } <<"END_SCRIPT";
#!/bin/sh
SVN_USER=\$1
AUTHOR="\$SVN_USER <\$SVN_USER\@svn-converter>"
echo "\$SVN_USER = \$AUTHOR" >> "$authors_file"
echo "\$AUTHOR"
END_SCRIPT
  close $fh;
  chmod 0755, "$repo/.git/author-generate";

  for my $config (keys %config) {
    if (ref $config{$config}) {
      eval { $git->config({unset_all => 1}, "svn-converter.$config") };
      $git->config({add => 1}, "svn-converter.$config", $_)
        for @{ $config{$config} };
    }
    else {
      $git->config("svn-converter.$config", $config{$config});
    }
  }
}

for my $attr (qw(
  talisman tag_prefix
)) {
  my $config_name = $attr;
  $config_name =~ s/_/-/g;
  has $attr => (
    is => 'lazy',
    default => sub { eval { ($_[0]->git->config("svn-converter.$config_name"))[0] } },
  );
}

for my $attr (qw(
  graft_revs extra_refs
)) {
  my $config_name = $attr;
  $config_name =~ s/_/-/g;
  has $attr => (
    is => 'lazy',
    default => sub {
      my %config;
      eval {
        for my $config ($_[0]->git->config('--get-all', "svn-converter.$config_name")) {
          my ($name, $value) = split /:/, $config, 2;
          $config{$name} = $value;
        }
      };
      \%config
    },
  );
}

has svn_url => (is => 'lazy', default => sub { $_[0]->git->config('svn-remote.svn.url') });

has grafts => (is => 'lazy', default => sub {
  my $graft_file = $_[0]->repo . '/.git/info/grafts';
  return {}
    unless -e $graft_file;
  open my $fh, '<', $graft_file;
  my @grafts = <$fh>;
  chomp @grafts;
  my %grafts;
  for my $graft (@grafts) {
    next
      if $graft =~ /^\s*(#|$)/;
    my ($commit, @parents) = split /\s+/, $graft;
    $grafts{$commit} = \@parents;
  }
  return \%grafts;
});
sub save_grafts {
  my $self = shift;
  my $grafts = $self->grafts;
  open my $fh, '>', realpath($self->repo) . '/.git/info/grafts';
  for my $commit (sort keys %$grafts) {
    print { $fh } join(' ', $commit, @{ $grafts->{$commit} }) . "\n";
  }
  close $fh;
}
has simplified_merges => (is => 'lazy', default => sub {
  my $merge_file = $_[0]->repo . '.git/info/simplified-merges';
  return {}
    unless -e $merge_file;
  open my $fh, '<', $merge_file;
  my $content = do { local $/; <$fh> };
  return decode_json($content);
});
sub save_simplified_merges {
  my $self = shift;
  my $merges = $self->simplified_merges;
  open my $fh, '>', $self->repo . '/.git/info/simplified-merges';
  print { $fh } encode_json($merges);
  close $fh;
}
sub add_simplified_merges {
  my ($self, $merges) = @_;
  my $simplified = $self->simplified_merges;
  my $grafts = $self->grafts;
  for my $merge ( keys %$merges ) {
    $simplified->{$merge} ||= $merges->{$merge};
    $grafts->{$merge} = $merges->{$merge};
  }
  $self->save_simplified_merges;
  $self->save_grafts;
}

has authors_file => (
  is => 'lazy',
  default => sub { 
    eval { ($_[0]->git->config('svn.authorsfile'))[0] };
  },
);

sub update {
  my $self = shift;
  if ($self->svn_url =~ /^file:/) {
    my %info = svn_info($self->svn_url);
    system 'svnsync', '--non-interactive', 'sync', $info{'Repository Root'};
  }
  my $repo = realpath($self->repo);
  local $ENV{PERL5LIB};
  local $ENV{PERL5OPT};
  local $CWD = $repo;
  system 'git', 'svn', 'fetch', "--authors-prog=$repo/.git/author-generate";
}

sub branches {
  my $self = shift;
  return [ $self->git->for_each_ref({format=>'%(refname)'}, 'refs/heads/convert/') ];
}
sub tags {
  my $self = shift;
  return [ $self->git->for_each_ref({format=>'%(refname)'}, 'refs/tags/convert/') ];
}
sub refs {
  my $self = shift;
  return [ $self->git->for_each_ref({format=>'%(refname)'}, 'refs/heads/convert', 'refs/tags/convert/') ];
}

sub rev_to_commit {
  my ($self, $rev) = @_;
  $rev =~ s/^r//;
  my ($commit) = $self->git->RUN('log', {format => '%H', grep => "git-svn-id:.*\@$rev "}, '-n', 1, @{$self->refs});
  return $commit
    if $commit;
  return;
}

sub commit_to_filtered {
  my ($self, $commit) = @_;
  return
    unless $commit;
  open my $fh, '<', $self->repo . '/.git/filter-map'
    or return $commit;
  while (my $line = <$fh>) {
    chomp $line;
    my ($orig, $filtered) = split /\s+/, $line;
    return $filtered
      if $orig eq $commit;
  }
  return $commit;
}

sub abbrev_commit {
  my ($self, $commit) = @_;
  return
    unless $commit;
  my ($abbrev) = $self->git->rev_parse({short => 1}, $commit);
  $abbrev;
}

sub cleanup {
  my $self = shift;
  my $git = $self->git;
  $git->update_ref('-d', $_)
    for $git->for_each_ref({format=>'%(refname)'},
      'refs/heads/convert/',
      'refs/tags/convert/',
      'refs/original/refs/heads/convert/',
      'refs/original/refs/tags/convert/',
    );
  eval { unlink $self->repo.'/.git/info/grafts' };
  eval { unlink $self->repo.'/.git/info/filter-map' };
  eval { unlink $self->repo.'/.git/info/simplified-merges' };
}

sub convert_refs {
  my $self = shift;

  my $git = $self->git;

  my @refs = $git->for_each_ref({format=>'%(refname)'}, 'refs/remotes/svn/');
  for my $ref (@refs) {
    (my $bare = $ref) =~ s{^refs/remotes/svn/}{};
    next
      if $bare =~ /\@/;
    next
      if $bare =~ m{^tags/};
    if (my $talisman = $self->talisman) {
      next
        unless eval { $git->show("$ref:$talisman"); 1 };
    }
    if ($bare eq 'trunk') {
      $bare = 'master';
    }
    $git->update_ref("refs/heads/convert/$bare", $ref);
  }
  my $extra_refs = $self->extra_refs;
  for my $extra ( keys %$extra_refs ) {
    my $commit = $self->rev_to_commit($extra_refs->{$extra});
    $git->update_ref("refs/heads/convert/$extra", $commit);
  }
}

sub convert_tags {
  my $self = shift;

  my $git = $self->git;
  my @refs = $git->for_each_ref({format=>'%(refname)'}, 'refs/remotes/svn/tags/');
  for my $ref (@refs) {
    (my $bare = $ref) =~ s{^refs/remotes/svn/tags/}{};
    next
      if $bare =~ /\@/;

    if (my $talisman = $self->talisman) {
      next
        unless eval { $git->show("$ref:$talisman"); 1 };
    }

    my $tag_name = $bare;
    if (my $prefix = $self->tag_prefix) {
      $tag_name =~ s/^$prefix-?//;
    }
    $tag_name =~ s/^v?/v/;

    $self->move_tag("convert/$tag_name", $ref, $ref);
  }
}

sub move_tag {
  my ($self, $tag, $ref, $orig) = @_;
  my $git = $self->git;

  my $commit_message;
  $orig ||= $tag;

  my %header;
  my ($type) = $git->cat_file({t => 1}, $orig);
  for my $line ($git->cat_file($type, $orig)) {
    if (defined $commit_message) {
      $commit_message .= $line . "\n";
    }
    elsif ($line eq '') {
      $commit_message = '';
    }
    else {
      my ($key, $value) = split / /, $line, 2;
      $header{$key} = $value;
    }
  }

  my $user = $header{tagger} || $header{committer};
  $user =~ s/\s+(\d+\s+[-+]?\d+)$//;
  my $date = $1;
  my ($name, $email) = $user =~ /^(.*?)\s+<([^<>]+)>$/;
  ($name, $email) = $self->fix_author($name, $email);

  $commit_message =~ s/^git-svn-id:.*//ms;
  $commit_message =~ s/\n+$//;

  local $ENV{GIT_COMMITTER_NAME} = $name;
  local $ENV{GIT_COMMITTER_EMAIL} = $email;
  local $ENV{GIT_COMMITTER_DATE} = $date;
  $git->tag({ f => 1, a => 1, message => $commit_message }, $tag, $ref);
}

sub fix_tags {
  my $self = shift;
  for my $tag (@{ $self->tags }) {
    (my $tag_name = $tag) =~ s{^refs/tags/convert/}{};
    $self->move_tag($tag_name, $tag, $tag);
  }
}

sub fix_author {
  my ($self, $name, $email) = @_;
  open my $fh, '<', $self->authors_file;
  while (my $line = <$fh>) {
    if ( $line =~ /(\S+)\s*=\s*(.*?)\s*<([^>]+)>/ ) {
      if ($1 eq $name) {
        return ("$2", "$3");
      }
    }
  }
  return ($name, $email);
}

sub log_search {
  my ($self, $search, %opts) = @_;
  return $self->git->RUN('log', {
    format => '%H',
    ($search ? ( 'grep' => $search ) : () ),
    %opts,
  }, @{ $self->refs });
}

sub graft_svk_merges {
  my $self = shift;
  my $git = $self->git;
  my @merges = $self->log_search('(orig r', no_merges => 1);

  my $grafts = $self->grafts;

  for my $commit (@merges) {
    my $commit_data = join "\n", $git->cat_file('commit', $commit);
    my @matched = $commit_data =~ /^[ ]r\d+\@[^\n]+\(orig[ ]r(\d+)\)/msxg;
    my ($parent_rev) = sort { $b <=> $a } @matched;
    unless ($parent_rev) {
      @matched = $commit_data =~ /^[ ][ ]r\d+\@[^\n]+\(orig[ ]r(\d+)\)/msxg;
      ($parent_rev) = sort { $b <=> $a } @matched;
      unless ($parent_rev) {
        @matched = $commit_data =~ /^[ ][ ][ ]r\d+\@[^\n]+\(orig[ ]r(\d+)\)/msxg;
        ($parent_rev) = sort { $b <=> $a } @matched;
        unless ($parent_rev) {
          warn "odd commit $commit.  merge but wrong format\n";
          next;
        }
      }
    }
    my @parents = $self->log_search("git-svn-id: .*\@$parent_rev ", 'E' => 1);
    if (@parents > 1) {
      my ($commit_branch) = $commit_data =~ /git-svn-id: ([^@]+)/;
      my @branch_parent_commits = $self->log_search("git-svn-id: $commit_branch\@$parent_rev ");
      if (@branch_parent_commits > 1) {
        @branch_parent_commits = $self->log_search("git-svn-id: .*/trunk\@$parent_rev ");
        unless (@branch_parent_commits == 1) {
          warn "odd commit $commit.  parent rev crosses branches\n";
          next;
        }
      }
      elsif (!@branch_parent_commits) {
        warn "odd commit $commit.  parent rev crosses branches\n";
        next;
      }
      @parents = @branch_parent_commits;
    }
    unless (@parents) {
      warn "can't find parent commit for $commit - r$parent_rev\n";
      next;
    }

    my ($parent) = @parents;

    my $commit_hashes = $git->show({s => 1, format => '%H %P'}, $commit);
    my ($commit_hash, @parent_hashes) = split /\s+/, $commit_hashes;

    if (! grep { $parent eq $_ } @parent_hashes ) {
      $grafts->{$commit_hash} = [@parent_hashes, $parent];
    }
  }
  $self->save_grafts;
}

sub merges {
  my $self = shift;
  return [
    map { [split / /] }
    $self->git->RUN('log', { merges => 1, format => '%H %P' }, @{$self->refs})
  ];
}
sub graft_simplified_merges {
  my $self = shift;
  my $git = $self->git;

  my @merges = @{ $self->merges };

  my %altered;
  my %merges;
  for my $merge ( reverse @merges ) {
    my ($commit, @parents) = @$merge;
    $merges{$commit} = \@parents;
    my ( $left_parent, $right_parent ) = @parents;
    # first parent is a merge
    if ( my $left_grandparents = $merges{ $left_parent } ) {
      my @grandparents = $git->show({s => 1, format => '%P'}, $right_parent);
      my ($right_grandparent) = split / /, @grandparents;
      if ($right_grandparent eq $left_grandparents->[1]) {
        $altered{$commit}++;
        $parents[0] = $left_grandparents->[0];
        delete $merges{ $left_parent };
        delete $altered{ $left_parent };
      }
    }
  }

  for my $commit ( keys %altered ) {
    $altered{$commit} = $merges{$commit};
  }
  $self->add_simplified_merges(\%altered);
}

sub graft_redundant_merges {
  my $self = shift;
  my $git = $self->git;

  my @merges = @{ $self->merges };

  my %altered;
  my %merges;
  for my $merge ( reverse @merges ) {
    my ($commit, @parents) = @$merge;
    $merges{$commit} = \@parents;
    PARENT: for my $p ( 0 .. 1 ) {
      my $parent = $parents[ $p ];
      my $check_ancest = $parents[ 1 - $p ];
      my $ancest = $merges{ $check_ancest } || next;

      ANCEST: for my $c ( 0 .. 1 ) {
        if ($parent eq $ancest->[ $c ]) {
          $altered{$commit}++;
          $parents[1 - $p] = $ancest->[1 - $c];
          delete $merges{ $check_ancest };
          delete $altered{ $check_ancest };
          last PARENT;
        }
      }
    }
  }

  for my $commit ( keys %altered ) {
    $altered{$commit} = $merges{$commit};
  }
  $self->add_simplified_merges(\%altered);
}

my $empty_tree = '4b825dc642cb6eb9a060e54bf8d69288fbee4904';
sub graft_remerges {
  my $self = shift;
  my $git = $self->git;

  my $grafts = $self->grafts;
  for my $merge ( @{ $self->merges } ) {
    my ($commit, @parents) = @$merge;
    my $base_tree = $empty_tree;
    eval {
      my ($base) = $self->git->merge_base(@parents);
      ($base_tree) = $self->git->rev_parse("$base^{tree}");
    };
    my @new_parents = grep {
      my ($parent_tree) = $git->rev_parse("$_^{tree}");
      $parent_tree eq $base_tree ? () : $_;
    } @parents;
    if (@new_parents != @parents) {
      $grafts->{$commit} = \@new_parents;
    }
  }
  $self->save_grafts;
}

sub graft_revisions {
  my $self = shift;
  my $git = $self->git;

  my $grafts = $self->grafts;
  my $graft_revs = $self->graft_revs;
  for my $graft_rev (keys %$graft_revs) {
    my ($commit, @parents) = map {
      $self->rev_to_commit($_) || do { warn "can't find $_"; next }
    } ($graft_rev, $graft_revs->{$graft_rev});
    $grafts->{$commit} = \@parents;
  }
  $self->save_grafts;
}

sub delete_merged_branches {
  my $self = shift;
  my $git = $self->git;
  my @branches = grep { $_ ne 'refs/heads/convert/master' } @{ $self->branches };
  my @refs = @{ $self->refs };
  my %remove;
  CHILD: for my $child ( @branches ) {
    for my $branch ( @refs ) {
      next if $child eq $branch;
      eval {
        $git->merge_base({is_ancestor => 1}, $child, $branch);
        ++$remove{$child};
      } and next CHILD;
    }
  }
  for my $branch ( keys %remove ) {
    $git->update_ref({d => 1}, $branch);
  }
}

sub delete_empty_branches {
  my $self = shift;
  my $git = $self->git;
  my @branches = grep { $_ ne 'refs/heads/convert/master' } @{ $self->branches };
  my @refs = @{ $self->refs };
  my %remove;
  CHILD: for my $child ( @branches ) {
    BRANCH: for my $branch ( @refs ) {
      next if $child eq $branch;
      my $base;
      eval {
        ($base) = $git->merge_base($child, $branch);
      };
      if ($base) {
        my @trees = $git->RUN('log', {format => '%T'}, "$base..$child");
        my ($base_tree) = $git->rev_parse("$base^{tree}");
        for my $tree (@trees) {
          if ($base_tree ne $tree) {
            next BRANCH;
          }
        }
        $remove{$child}++;
        next CHILD;
      }
    }
  }
  for my $branch ( keys %remove ) {
    $git->update_ref({d => 1}, $branch);
  }
}

sub archive_deleted_branches {
  my $self = shift;
  my $git = $self->git;
  my ($branch_location) = @{ $self->branch_roots };

  my $branch_root = $self->svn_url . '/' . $branch_location;

  my @branches = @{ $self->branches };
  for my $branch (@branches) {
    (my $bare = $branch) =~ s{^refs/heads/convert/}{};
    next
      if $bare =~ m{^trash/};
    my $url = "$branch_root/$bare";
    if (! eval { system "svn info $url >/dev/null 2>/dev/null"; 1 }) {
      $git->update_ref("refs/heads/convert/trash/$bare", $branch);
      $git->update_ref({d => 1}, $branch);
    }
  }
}

sub bake {
  my $self = shift;
  my $repo = $self->repo;
  my @refs = @{ $self->refs };
  chdir $repo;
  system 'git', 'filter-branch',
    "--tree-filter"
      => "perl -MSVN::GitConverter::FilterCmd -e make_ignores " . $self->repo,
    "--msg-filter"
      => "perl -MSVN::GitConverter::FilterCmd -e msg_filter " . $self->repo,
    "--env-filter"
      => 'eval $(perl -MSVN::GitConverter::FilterCmd -e calculate-author '.$self->repo.')',
    "--commit-filter"
      => 'NEW_COMMIT=$(git_commit_non_empty_tree "$@"); echo $GIT_COMMIT $NEW_COMMIT >> ' . $self->repo.'/.git/filter-map; echo $NEW_COMMIT',
    "--tag-name-filter"
      => "cat",
    "--force",
    "--", @refs,
  ;
}

sub filter_message {
  my ($self, $commit, $message) = @_;
  if ($self->simplified_merges->{$commit}) {
    $message = '';
  }
  $message =~ s/^git-svn-id: .*\n?//m;
  $message =~ s/\A\s*-\s*([^\n]+)\n*\z/$1/ms;
  $message =~ s{\b((r\d+)(?::(\d+)))\b}{
    my $sha = $self->abbrev_commit($self->commit_to_filtered($self->rev_to_commit($2)));
    my $out = $1;
    if ($sha && $3) {
      my $sha2 = $self->abbrev_commit($self->commit_to_filtered($self->rev_to_commit($3)));
      if ($sha2) {
        $out = "$sha..$sha2";
      }
    }
    elsif ($sha) {
      $out = $sha;
    }
    $out;
  }ge;
  my $git = $self->git;
  my ( $to, @from ) = split /\s+/, ($git->show({ s => 1, format => '%P' }, $commit))[0];
  $message =~ s/\s+\z/\n/;

  if ( @from ) {
    # print the original message
    $message =~ s/^ //mg;

    return "Merge ",
        join(", ", map { $self->format_branch($_) } @from),
        " into ",
        $self->format_branch($to),
        "\n\n",
        $message;
  }
  else {
    if ( $message =~ s/^\s*r\d+\@.*:.*$//m ) {
      $message =~ s/^ //mg;
    }
    return $message;
  }
}

sub format_branch {
  my ($self, $commit) = @_;
  my $git = $self->git;
  my ($svn_id) = git_svn_id($git->show({s => 1, format => '%s%n%n%b'}, $commit));
  my $svn_url = ($self->git->config('svn-remote.svn.rewriteRoot'))[0] || $self->svn_url;
  $svn_id =~ s{^\Q$svn_url\E/?}{};
  my $rev = $1;
  my @roots = ('('.$self->trunk.')', map { "$_/([^/]+)" } @{ $self->branch_roots });
  for my $root (@roots) {
    if ($svn_id =~ /^$root/) {
      return "'$1'";
    }
  }
  if ($rev) {
    return $rev;
  }
  die "unknown rev for $commit";
}
