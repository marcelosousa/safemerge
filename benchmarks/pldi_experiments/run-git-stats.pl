#!/usr/bin/perl

use strict;
use warnings;

my $file = 'repos.txt';

open my $info, $file or die "Could not open $file: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  print "Processing $words[0]";
#  system("git clone $words[1]");
  system("cd $words[0]; git-merge-analyser > gma.log");
}

close $info;

