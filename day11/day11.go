package main

import (
	"fmt"
	"math/big"
	"os"
	"sort"
	"strconv"
	"strings"
)

var zero = big.NewInt(0)

type Monkey struct {
	number    int
	items     []*big.Int
	operation func(old *big.Int) *big.Int
	test      func(item *big.Int) int
	inspected int
}

func (m Monkey) String() string {
	return fmt.Sprintf("Monkey %d {%v}", m.number, m.items)
}

func copyInt(in *big.Int) *big.Int {
	temp := big.NewInt(0)
	temp.Set(in)
	return temp
}

func ParseMonkey(input string) Monkey {
	var m Monkey

	lines := strings.Split(input, "\n")

	n, err := strconv.Atoi(strings.Split(lines[0][:len(lines[0])-1], " ")[1])
	if err != nil {
		panic(err)
	}
	m.number = n

	for _, item := range strings.Split(lines[1][len("  Starting items: "):], ", ") {
		n, err := strconv.Atoi(item)
		if err != nil {
			panic(err)
		}

		m.items = append(m.items, big.NewInt(int64(n)))
	}

	operators := strings.Split(lines[2][len("  Operation: new = "):], " ")
	var firstOperator, secondOperator int

	firstOld := operators[0] == "old"
	secondOld := operators[2] == "old"

	if !firstOld {
		n, err := strconv.Atoi(operators[0])
		if err != nil {
			panic(err)
		}
		firstOperator = n
	}

	if !secondOld {
		n, err := strconv.Atoi(operators[2])
		if err != nil {
			panic(err)
		}
		secondOperator = n
	}

	var operatorFunc func(x, y *big.Int) *big.Int
	switch rune(operators[1][0]) {
	case '+':
		operatorFunc = func(x, y *big.Int) *big.Int { return x.Add(x, y) }
	case '*':
		operatorFunc = func(x, y *big.Int) *big.Int { return x.Mul(x, y) }
	}

	m.operation = func(old *big.Int) *big.Int {
		first := big.NewInt(int64(firstOperator))
		second := big.NewInt(int64(secondOperator))

		if firstOld {
			first = old
		}

		if secondOld {
			second = old
		}

		return operatorFunc(copyInt(first), second)
	}

	divisible, err := strconv.Atoi(lines[3][len("  Test: divisible by "):])
	if err != nil {
		panic(err)
	}

	throwTrue, err := strconv.Atoi(lines[4][len("    If true: throw to monkey "):])
	if err != nil {
		panic(err)
	}

	throwFalse, err := strconv.Atoi(lines[5][len("    If false: throw to monkey "):])
	if err != nil {
		panic(err)
	}

	bigDiv := big.NewInt(int64(divisible))

	m.test = func(item *big.Int) int {
		temp := copyInt(item)
		if temp.Rem(temp, bigDiv).Cmp(zero) == 0 {
			return throwTrue
		}
		return throwFalse
	}

	return m
}

func main() {
	in, err := os.ReadFile("input")
	if err != nil {
		panic(err)
	}

	monkeys := []Monkey{}

	for _, block := range strings.Split(string(in), "\n\n") {
		monkeys = append(monkeys, ParseMonkey(block))
	}

	for iter := 0; iter <= 10_000; iter++ {
		for mi, monkey := range monkeys {
			for _, item := range monkey.items {
				//fmt.Printf("Inspecting item with %v\n", item)

				new_worry := monkey.operation(item)
				//fmt.Printf("Item is now %v\n", new_worry)

				//new_worry /= 3
				//fmt.Printf("Divided by 3 to %v\n", new_worry)

				ind := monkey.test(new_worry)
				//fmt.Printf("Throwing item %v to monkey %v\n", new_worry, ind)
				monkeys[ind].items = append(monkeys[ind].items, new_worry)

				monkeys[mi].inspected++
			}
			monkeys[mi].items = []*big.Int{}
		}
		fmt.Printf("Completed iter %d\n", iter)
	}

	//fmt.Println(monkeys)

	inspected := []int{}

	for _, monkey := range monkeys {
		inspected = append(inspected, monkey.inspected)
	}

	sort.Sort(sort.IntSlice(inspected))

	fmt.Println(inspected)

	fmt.Printf("Monkey Business: %d\n", inspected[len(inspected)-1]*inspected[len(inspected)-2])
}
