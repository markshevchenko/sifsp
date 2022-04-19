#include <math.h>
#include <stdio.h>

// Chapter 1

double square(double x) { return x * x; }

double cube(double x) { return x * x * x; }

double average(double a, double b) { return (a + b) / 2; }

double sqrt1(double x)
{
    double guess = 1;

    while (fabs(square(guess) - x) >= 0.001)
        guess = average(guess, x / guess);

    return guess;
}

double fact(unsigned n)
{
    double result = 1;

    while (n > 0) {
        result *= n;
        n--;
    }

    return result;
}

double fast_power(double base, unsigned n)
{
    double result = 1;
    double factor = base;

    while (n > 0) {
        if (n % 2 == 1)
            result *= factor;

        factor *= factor;
        n /= 2;
    }

    return result;
}

double integral(double (*f)(double), double a, double b, double dx)
{
    double sum = 0;

    for (double x = a; x <= b; x += dx)
        sum += f(x + dx / 2) * dx;

    return sum;
}

double search_zero(double (*f)(double), double neg, double pos)
{
    double mid = average(neg, pos);

    while (fabs(neg - pos) >= 0.001) {
        double test_value = f(mid);

        if (test_value < 0)
            pos = mid;
        else if (test_value > 0)
            neg = mid;
        else
            break;

        mid = average(neg, pos);
    }

    return mid;
}

double fixed_point(double (*f)(double), double guess)
{
    while(1) {
        double next = f(guess);
        if (fabs(next - guess) < 0.00001)
            return next;

        guess = next;
    }
}

double golden_section_f(double x)
{
    return 1 + 1 / x;
}

int main()
{
    printf("Chapter I\n");
    printf("square(2) = %f\n", square(2));
    printf("cube(2) = %f\n", cube(2));
    printf("average(1, 2 / 1) = %f\n", average(1, 2 / 1));
    printf("sqrt1(2) = %f\n", sqrt1(2));
    printf("fact(10) = %f\n", fact(10));
    printf("fast_power(2, 10) = %f\n", fast_power(2, 10));
    printf("integral(cube, 0, 1, 0.01) = %f\n", integral(cube, 0, 1, 0.01));
    printf("pi = search_zero(sin, 2, 4) = %f\n", search_zero(sin, 2, 4));
    printf("fixed_point(cos, 1) = %f\n", fixed_point(cos, 1));
    printf("golden section = fixed_point(golden_section_f, 1) = %f\n", fixed_point(golden_section_f, 1));
}
