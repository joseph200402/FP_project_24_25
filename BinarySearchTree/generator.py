import random

def generate_command(file_name, num_values, value_range):
    """
    Generates a file with random numbers based on user input.

    Args:
        file_name (str): The name of the output file.
        num_values (int): The number of random values to generate.
        value_range (int): The upper limit of the random range (1 to value_range).
    """
    numbers = [str(random.randint(1, value_range)) for _ in range(num_values)]
    
    with open(file_name, "w") as file:
        for number in numbers:
            file.write(number + "\n")

    print(f"File '{file_name}' generated with {num_values} random numbers ranging from 1 to {value_range}.")

if __name__ == "__main__":
    # Ask the user for inputs
    file_name = input("Enter the file name: ").strip()
    try:
        num_values = int(input("Enter the number of values to generate: ").strip())
        value_range = int(input("Enter the range (from 1 to _): ").strip())

        # Generate the file
        generate_command(file_name, num_values, value_range)
    except ValueError:
        print("Invalid input. Please enter valid numbers for the values and range.")
