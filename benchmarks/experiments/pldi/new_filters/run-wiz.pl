#!/usr/bin/perl

use strict;
use warnings;

open(my $info, '-|', 'find', 'safe', '-type', 'f', '-name', '*_o.java', '-print') or die "Failed: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  my $bench = substr($words[0],0,-7);
  my @dirs = split '/', $bench;
  # my $dir = "$dirs[0]/$dirs[1]/$dirs[2]/diff.txt";
  print "Processing $bench\n";
 # system("wiz diff4 -b $bench > $dir");
  system("wiz verify -b $bench -m=Dep");
}

close $info;

