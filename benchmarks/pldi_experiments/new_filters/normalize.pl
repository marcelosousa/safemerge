#!/usr/bin/perl

use strict;
use warnings;

my $file = 'list.txt';

open my $info, $file or die "Could not open $file: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  system("mv $words[0]\/results\/cat9 $words[0]\/results");
  system("rm -rf $words[0]\/results");
  system("rm -rf $words[0]\/LComplex");
  system("rm -rf $words[0]\/LStateless");
}

close $info;

