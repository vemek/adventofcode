#!/usr/bin/env ruby

require "pry"

two_repeats = 0
three_repeats = 0

input = File.open("day-2-input.txt")
input.each_line do |box_id|
  counts = Hash.new { |h, k| h[k] = 0 }
  box_id.each_char { |c| counts[c] += 1 }
  two_repeats += 1 if counts.value?(2)
  three_repeats += 1 if counts.value?(3)
end

puts "checksum: #{two_repeats * three_repeats}"
