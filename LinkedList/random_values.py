import random

def generate_command():
    numbers = [str(random.randint(1, 20000)) for _ in range(500000)]
    return numbers

if __name__ == "__main__":
    numbers = generate_command()
    with open("x5.txt", "w") as file:
        for number in numbers:
            file.write(number+"\n")