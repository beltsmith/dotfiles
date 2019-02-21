#! /usr/bin/env ruby

require 'knapsack'

scenarios = [
  {
    name: 'units',
    pattern: 'spec/{apis,controllers,engines,jobs,javascripts,helpers,lib,mailers,models,presenters,scripts,splits,views}/**/*_spec.rb',
    total: '10'
  },
  {
    name: 'features',
    pattern: 'spec/features/**/*_spec.rb',
    total: '12'
  }
]

ENV['KNAPSACK_REPORT_PATH'] = 'spec/knapsack/report.json'

scenarios.each do |scenario|
  ENV['KNAPSACK_TEST_FILE_PATTERN'] = scenario[:pattern]
  node_total = ENV['CI_NODE_TOTAL'] = scenario[:total]

  node_map = node_total.to_i.times.each_with_object({}) do |i, ret|
    ENV['CI_NODE_INDEX'] = i.to_s
    allocator = Knapsack::AllocatorBuilder.new(Knapsack::Adapters::RSpecAdapter).allocator

    allocator.node_tests.each do |test|
      ret[test] = i
    end
  end

  File.open("knapsacked_#{scenario[:name]}_by_node.json", 'w') do |f|
    f.puts Hash[node_map.group_by(&:last).map { |k, v| [k, v.map(&:first)] }].to_json
  end
  File.open("knapsacked_#{scenario[:name]}.json", 'w') do |f|
    f.puts node_map.to_json
  end
end
