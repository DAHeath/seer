from faker import Faker

fake = Faker()

for i in range(1,10):
    print fake.email()
