
from main import parse  
from main import lexer

# Programa de prueba en MyR
test_program = """
Program MyR

vars
    int i,j,k,arreglo[10];
    flot promedio;
    char Emilio, German;
    
function flot Prom3Num(int uno, int dos, int tres) {
vars 
    flot promedio;
promedio = (uno + dos + tres)/3;
return(promedio);
}

main() {
i = 1;
j = 2;
k = 3;    
Emilio = Hola;
German = Adios; 
promedio = Prom3Num(i,j,k);    
write(promedio);
for i = 0 to 9 do {
    promedio = Prom3Num(i,j,k);
}

if (promedio > 5) then {
write (Emilio);
} else {
write (German);
}

}
"""


def run_test():
    result = parse(test_program)
    print("Resultado del Análisis:")
    print(result)

if __name__ == "__main__":
    run_test()
