#!/usr/bin/env ruby

require "pry"

box_ids = []

File.open("day-2-input.txt").each_line { |box_id| box_ids << box_id.chomp }

def levenshtein_distance(s, t)
  m = s.length
  n = t.length
  return m if n == 0
  return n if m == 0
  d = Array.new(m+1) {Array.new(n+1)}

  (0..m).each {|i| d[i][0] = i}
  (0..n).each {|j| d[0][j] = j}
  (1..n).each do |j|
    (1..m).each do |i|
      d[i][j] = if s[i-1] == t[j-1]  # adjust index into string
                  d[i-1][j-1]       # no operation required
                else
                  [ d[i-1][j]+1,    # deletion
                    d[i][j-1]+1,    # insertion
                    d[i-1][j-1]+1,  # substitution
                  ].min
                end
    end
  end
  d[m][n]
end

lowest_edit_distance = 9999
box_ids.combination(2) do |box_id_1, box_id_2|
	edit_distance = levenshtein_distance(box_id_1, box_id_2)
	if edit_distance < lowest_edit_distance
		lowest_edit_distance = edit_distance
		puts "New lowest edit distance (#{lowest_edit_distance}) between #{box_id_1} and #{box_id_2}"
	end
end

