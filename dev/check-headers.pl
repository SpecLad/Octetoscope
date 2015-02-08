#!/usr/bin/env perl

# Add "perl dev/check-headers.pl || exit 1" to your pre-commit hook
# to make sure your files have up-to-date copyright/license headers.
# This script is placed into the public domain.

use strict;
use warnings;

use Cwd qw/abs_path/;
use File::Basename;
use File::Spec;
use List::Util qw/first/;

my $re_year = qr/[1-9]\d{3,}/;
my $re_year_range = qr/$re_year|$re_year-$re_year/;
my $re_years = qr/$re_year_range(?:, $re_year_range)*/;

sub has_suffix {
  my @suffixes = @_;

  return sub {
    my ($path) = @_;
    for (@suffixes) {
      return 1 if $path =~ /\Q$_\E$/;
    }
    return 0;
  }
}

my @rules;

sub fallback {
  my ($value, $fallback) = @_;
  return defined($value) ? $value : $fallback;
}

sub rule {
  my ($predicate, $header, %options) = @_;
  push @rules, {PREDICATE => $predicate, HEADER => $header,
                BEFORE => fallback($options{BEFORE}, ''),
                AFTER => fallback($options{AFTER}, ''),
                PREFIX => fallback($options{PREFIX}, '')};
}

my $script_dir = [fileparse(abs_path($0))]->[1];
my $conf_file = File::Spec->catfile($script_dir, 'check-headers.conf');

unless (defined do $conf_file) {
  die "couldn't parse \"$conf_file\": $@" if $@;
  die "couldn't read \"$conf_file\": $!";
}

my $exit_status = 0;

sub get_copyright_years {
  my ($fname) = @_;

  my $rule = first { $_->{PREDICATE}->($fname) } @rules;

  return undef unless defined $rule;

  my $empty_line_prefix = $rule->{PREFIX};
  $empty_line_prefix =~ s/\s+$//;

  my $header = $rule->{HEADER};
  $header =~ s/^(?=.)/$rule->{PREFIX}/mg;
  $header =~ s/^$/$empty_line_prefix/mg;
  $header = quotemeta($header);
  $header =~ s/YEARS/($re_years)/;
  $header = qr/^\Q$rule->{BEFORE}\E$header\Q$rule->{AFTER}\E/;

  open my $file, '-|', 'git', 'show', ':' . $fname;

  my $contents = do { local $/; <$file> };

  close $file;

  if ($contents !~ $header) {
    print STDERR "$fname: missing/wrong license header\n";
    $exit_status = 1;
    return undef;
  }

  return $1;
}

my $current_year = (gmtime)[5] + 1900;

my @changed_files = split /\0/, `git diff --staged --name-only --diff-filter=CMR -z`;

FILE: for my $fname (@changed_files) {
  my $years = get_copyright_years($fname);

  next unless defined $years;

  for my $year_range (split /, /, $years) {
    if ($year_range =~ /^(.*)-(.*)$/) {
      next FILE if $1 <= $current_year && $current_year <= $2;
    } else {
      next FILE if $year_range == $current_year;
    }
  }

  print STDERR "$fname: copyright years \"$years\" don't cover current year $current_year\n";
  $exit_status = 1;
}

my @added_files = split /\0/, `git diff --staged --name-only --diff-filter=A -z`;

for my $fname (@added_files) {
  my $years = get_copyright_years($fname);

  next unless defined $years;

  next if $years eq $current_year;

  print STDERR "$fname: copyright years for new file should be \"$current_year\", but is \"$years\" instead\n";
  $exit_status = 1;
}

exit $exit_status;
