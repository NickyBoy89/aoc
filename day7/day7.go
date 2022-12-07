package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Directory struct {
	name   string
	size   int
	parent *Directory
	files  []*Directory
}

func (d *Directory) FindFile(name string) *Directory {
	for _, file := range d.files {
		if file.name == name {
			return file
		}
	}
	return nil
}

func (d *Directory) AddFile(name string, size int) *Directory {
	newFile := &Directory{name: name, size: size}
	d.files = append(d.files, newFile)
	return newFile
}

func (d *Directory) DirectorySize() int {
	if len(d.files) == 0 {
		return d.size
	}

	var total int
	for _, file := range d.files {
		total += file.DirectorySize()
	}
	return total
}

func (d *Directory) GetDirectoriesAtMost(limit int) []int {
	if len(d.files) == 0 {
		return []int{}
	}

	dirs := []int{}
	if d.DirectorySize() <= limit {
		dirs = append(dirs, d.DirectorySize())
	}

	for _, file := range d.files {
		dirs = append(dirs, file.GetDirectoriesAtMost(limit)...)
	}

	return dirs
}

func (d *Directory) DirectorySizes() []int {
	if len(d.files) == 0 {
		return []int{}
	}

	dirs := []int{d.DirectorySize()}

	for _, file := range d.files {
		dirs = append(dirs, file.DirectorySizes()...)
	}

	return dirs
}

func (d Directory) String() string {
	return fmt.Sprintf("Dir{name=%s, size=%d, files=%v}", d.name, d.size, d.files)
}

func main() {
	in, err := os.ReadFile("input")
	if err != nil {
		panic(err)
	}

	root := Directory{name: "/", size: -1}

	cur := &root

	lines := strings.Split(string(in), "\n")
	lines = lines[:len(lines)-1]

	for index, line := range lines {
		// Command
		if line[0] == '$' {
			args := strings.Split(line, " ")
			switch args[1] {
			case "cd":
				command := args[len(args)-1]
				switch command {
				case "..":
					cur = cur.parent
				case "/":
					cur = &root
				default:
					found := cur.FindFile(command)
					if found != nil {
						cur = found
					} else {
						newFile := cur.AddFile(command, -1)
						newFile.parent = cur
						cur = newFile
					}
				}
			case "ls":
				for _, listing := range lines[index+1:] {
					if listing[0] == '$' {
						break
					}
					entry := strings.Split(listing, " ")

					if len(entry) != 2 {
						panic("Unexpected file listing size")
					}

					if entry[0] == "dir" {
						newFile := cur.AddFile(entry[1], -1)
						newFile.parent = cur
					} else {
						size, err := strconv.Atoi(entry[0])
						if err != nil {
							panic(err)
						}

						cur.AddFile(entry[1], size)
					}
				}
			default:
				panic(fmt.Errorf("Unknown command: %s", args[1]))
			}
		}
	}

	dirs := root.GetDirectoriesAtMost(100_000)

	var total int
	for _, size := range dirs {
		total += size
	}

	fmt.Println(total)

	sizes := root.DirectorySizes()
	sort.Sort(sort.IntSlice(sizes))

	totalSize := 70_000_000
	targetSize := 30_000_000

	rootSize := root.DirectorySize()
	unusedSpace := totalSize - rootSize

	fmt.Printf("Unused: %d\n", unusedSpace)

	for _, size := range sizes {
		if unusedSpace+size >= targetSize {
			fmt.Println(size)
			break
		}
	}
}
