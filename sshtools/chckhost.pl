#!/usr/bin/perl
use Net::Ping;
if( new Net::Ping()->ping( shift ) ) {
  print "yes";
}
