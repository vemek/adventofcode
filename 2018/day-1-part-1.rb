#!/usr/bin/env ruby

input = File.open("day-1-input.txt")
total = 0

input.each_line do |line|
  i = line.to_i
  total += i
end

puts "total: #{total}"
