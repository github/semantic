#!/usr/bin/env ruby

require 'yaml'

components = []

Dir.glob("./semantic*").each do |dir|
  components << {'path' => dir + '/src', 'component' => "lib:" + dir[2..]}
end

Dir.glob("./semantic*/test").each do |dir|
  components << {'path' => dir, 'component' => "test:" + dir[2..-6]}
end


result = {'cradle' => { 'cabal' => components }}
puts YAML.dump(result)
