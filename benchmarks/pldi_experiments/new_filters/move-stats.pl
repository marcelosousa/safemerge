#!/usr/bin/perl

use strict;
use warnings;

my $file = 'list.txt';

open my $info, $file or die "Could not open $file: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  system("rm -rf $words[0]");
  system("mkdir $words[0]");
  system("cp ..\/$words[0]\/gma.log $words[0]\/gma.log");
  system("cp -r ..\/$words[0]\/results\/cat9\/LSimple $words[0]\/");
  system("cp -r ..\/$words[0]\/results\/cat9\/LCond $words[0]\/");
  system("cp -r ..\/$words[0]\/results\/cat9\/LLoop $words[0]\/");
}

close $info;

