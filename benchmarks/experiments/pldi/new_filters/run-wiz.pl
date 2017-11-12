#!/usr/bin/perl

use strict;
use warnings;

open(my $info, '-|', 'find', '.', '-type', 'f', '-name', '*_o.java', '-print') or die "Failed: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  my $bench = substr($words[0],2,-7);
  my @dirs = split '/', $bench;
  my $dir = "$dirs[0]/$dirs[1]/$dirs[2]/diff.txt";
  print "Processing $dir\n";
  system("wiz diff4 -b $bench > $dir");
}

close $info;

