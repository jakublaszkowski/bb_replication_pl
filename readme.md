# Opis plików:

## backcast_oczekiwania.R

Plik wykonuje backcast oczekiwań długoterminowych na podstawie oczekiwań krótkoterminowych.

## estymacja.R

Plik zawiera polecenia, które oszacowują poszczególne równania. W tym celu pobierane są dane z pliku, w którym znajdują się dane, następnie wybierana jest interesująca próba. *Równania użyte później w symulacji są zapisywane w komentarzu po komendzie podsumowującej*. Dla każdego równania dokonano estymacji w czterech formach, z różną liczbą opóźnień, aby sprawdzić, która będzie najlepsza.

## simul_function.R

Plik zawiera funkcję, która wykonuje symulację na podstawie danych wejściowych oraz oszacowań parametrów strukturalnych z pliku estymacja.R.

## IRFs.R

Plik generuje IRF przy użyciu funkcji z pliku simul_function oraz na podstawie oszacowań zmiennych z pliku estymacja. Wyjściem pliku jest wykres z IRF dla szoków cen energii, żywności oraz niedoborów. Wszystkie szoki wprowadzone są w wielkości 1 odchylenia standardowego wewnątrz próby. Wyjściem pliku jest również wykres funkcji reakcji.

## simulation

Plik generuje symulację przy użyciu funkcji z pliku simul_function oraz na podstawie oszacowań zmiennych z pliku estymacja. Wyjściem pliku jest wykres z IRF.

Zakładane są wartości zmiennych egzogenicznych takie jak w danych, a wyznaczana jest ścieżka zmiennych endogenicznych. Wyjściem pliku jest wykres porównujący dane faktyczne i zasymulowane.

## org_equations

Replikacja estymacji z oryginalnej pracy. Sprawdzam czy zgadzają się sumy