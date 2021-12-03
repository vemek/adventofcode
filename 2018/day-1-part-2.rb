#!/usr/bin/env ruby

require "pry"
require "set"

total = 0
seen = Set[0]
first_repeat = nil

loop do
  input = File.open("day-1-input.txt")
  input.each_line do |line|
    i = line.to_i
    total += i
    puts total
    if seen.include?(total)
      first_repeat = total
      break
    end
    seen.add(total)
  end
  break unless first_repeat.nil?
end

puts "first repeated num: #{total}"
