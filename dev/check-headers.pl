#!/usr/bin/env perl

# Add "perl dev/check-headers.pl || exit 1" to your pre-commit hook
# to make sure your files have up-to-date copyright/license headers.
# This script is placed into the public domain.

use strict;
use warnings;

use File::Basename;

my $re_year = qr/[1-9]\d{3,}/;
my $re_year_range = qr/$re_year|$re_year-$re_year/;
my $re_years = qr/$re_year_range(?:, $re_year_range)*/;

my $generic_header = <<'EOF';
This file is part of Octetoscope.
Copyright (C) YEARS Octetoscope contributors (see /AUTHORS.txt)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
EOF

my @file_suffixes = (".gradle", ".groovy", ".java", ".scala");

my @changed_files = split /\0/, `git diff --staged --name-only --diff-filter=ACMR -z`;

my $exit_status = 0;

FILE: for my $fname (@changed_files) {
  next if (fileparse($fname, @file_suffixes))[2] eq '';

  # This transformation is not part of $generic_header in case
  # we later need to support languages with different comment syntax.
  my $header = $generic_header;
  $header =~ s/^(.)/  $1/mg;
  $header = quotemeta($header);
  $header =~ s/YEARS/($re_years)/;
  $header = qr"^/\*\n$header\*/\n";

  # print $header;

  open my $file, '-|', 'git', 'show', ':' . $fname;

  my $contents = do { local $/; <$file> };

  if ($contents !~ $header) {
    print STDERR "$fname: no license header\n";
    $exit_status = 1;
    next;
  }

  my $years = $1;
  my $current_year = (gmtime)[5] + 1900;

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

exit $exit_status;
