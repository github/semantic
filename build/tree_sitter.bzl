# To provide access to node-types.json files and the contents
# of the packages's corpus/ directory, we define a BUILD file
# that will be injected, via the build_file_contents, into
# the repositories that we download.

package(default_visibility = ["//visibility:public"])

exports_files(glob(["**/node-types.json"]))

filegroup(name = "corpus", srcs = glob(["**/corpus/*.txt"]))
