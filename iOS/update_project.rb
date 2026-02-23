#!/usr/bin/env ruby
require 'xcodeproj'

project_path = 'HyaloApp.xcodeproj'
project = Xcodeproj::Project.open(project_path)

# Find the HyaloKit target configuration
project.targets.each do |target|
  if target.name == 'HyaloKit'
    target.build_configurations.each do |config|
      # Set library search paths for simulator
      config.build_settings['LIBRARY_SEARCH_PATHS[sdk=iphonesimulator*]'] = [
        '$(inherited)',
        '$(SRCROOT)/../../hyalo-feedstock-unified/emacs/src',
        '$(SRCROOT)/../../hyalo-feedstock-unified/ios-sim-deps/lib'
      ]
      
      # Set library search paths for device
      config.build_settings['LIBRARY_SEARCH_PATHS[sdk=iphoneos*]'] = [
        '$(inherited)',
        '$(SRCROOT)/../../hyalo-feedstock-unified/emacs/src',
        '$(SRCROOT)/../../hyalo-feedstock-unified/ios-deps/lib'
      ]
      
      # Set linker flags for simulator
      config.build_settings['OTHER_LDFLAGS[sdk=iphonesimulator*]'] = [
        '$(inherited)',
        '-force_load',
        '$(SRCROOT)/../../hyalo-feedstock-unified/emacs/src/libemacs.a',
        '-lxml2',
        '-ljansson',
        '-lgmp',
        '-lgnutls',
        '-lnettle',
        '-lhogweed',
        '-ltasn1',
        '-ltree-sitter',
        '-lz',
        '-liconv'
      ]
      
      # Set linker flags for device
      config.build_settings['OTHER_LDFLAGS[sdk=iphoneos*]'] = [
        '$(inherited)',
        '-force_load',
        '$(SRCROOT)/../../hyalo-feedstock-unified/emacs/src/libemacs.a',
        '-lxml2',
        '-ljansson',
        '-lgmp',
        '-lgnutls',
        '-lnettle',
        '-ltasn1',
        '-lz',
        '-liconv'
      ]
      
      puts "Updated #{target.name} #{config.name} configuration"
    end
  end
end

project.save
puts "Project updated successfully"
