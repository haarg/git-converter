package SVN::GitConverter::FilterCmd;
use strictures 1;
use autodie ':all';

use Exporter qw(import);

our @EXPORT = qw(make_ignores msg_filter calculate_author);

sub make_ignores {
  my ($repo) = @ARGV;
  my $commit = $ENV{GIT_COMMIT};
  if (-e '.gitignore') {
    return;
  }

  require Git::Wrapper;
  require SVN::GitConverter;

  my $git = Git::Wrapper->new($repo);

  my ($svn_url) = $git->config('svn-remote.svn.url');
  my ($rewrite_url) = $git->config('svn-remote.svn.rewriteRoot');
  $svn_url =~ s{/?$}{/};

  my ($url, $rev) = SVN::GitConverter::git_svn_id($git->cat_file('commit', $commit));
  $url =~ s{\Q$rewrite_url\E/?}{$svn_url};
  if ($rev) {
    my @ignores;
    my $dir;
    open my $io, '-|', 'svn', 'pg', '-R', 'svn:ignore', "$url\@$rev";
    while (my $line = <$io>) {
      chomp $line;
      if ($line =~ s{^\Q$url\E(\S*) -\s+}{}) {
        $dir = $1;
        $dir =~ s{^/}{};
        $dir =~ s{/?$}{/};
      }
      next
        unless $line;
      my $file = "$dir$line";
      $file =~ s{^/}{};
      $file =~ s{/\*$}{/};
      $file =~ s{\b(_build|blib|inc|t/var)/?$}{$1/};
      push @ignores, $file;
    }
    close $io;
    if (@ignores) {
      open my $fh, '>', '.gitignore';
      print {$fh} "$_\n"
        for sort @ignores;
      close $fh;
    }
  }
}

sub msg_filter {
  my ($repo) = @ARGV;
  my $message = do { local $/; <STDIN> };
  my $commit = $ENV{GIT_COMMIT};
  require SVN::GitConverter;
  my $c = SVN::GitConverter->new(repo => $repo);
  print $c->filter_message($commit, $message);
}

sub calulate_author {
  my ($repo) = @ARGV;
  my $commit = $ENV{GIT_COMMIT};
  my $name = $ENV{GIT_AUTHOR_NAME};
  my $email = $ENV{GIT_AUTHOR_EMAIL};

  if ("$name <$email>" eq "$name <$name\@svn-converter>") {
    require SVN::GitConverter;
    my $c = SVN::Converter->new(repo => $repo);
    ($name, $email) = $c->fix_author($name, $email);
    print "export GIT_AUTHOR_NAME='$name'\n";
    print "export GIT_AUTHOR_EMAIL='$email'\n";
    print "export GIT_COMMITTER_NAME='$name'\n";
    print "export GIT_COMMITTER_EMAIL='$email'\n";
  }
}

1;
