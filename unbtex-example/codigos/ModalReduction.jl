module ModalReduction
export generateA, generateB, generateC
export generateABC, getABC_M, getABCD_R
export manuscript_p48, simulation
export generateMATLABSimulationScript

#Gera A, B, C to sistema completo
function generateABC(n)
    tau = 0.2426    # tau do barbante (1/s) para excursão de 30cm
    taul = 0.1133   # tau da bolinha (1/s) para excursão de 30cm
    ms = 0.0006     # massa linear do barbante (kg/m)
    mb = 0.00015    # massa da bolinha (kg)
    g = 9.80665     # aceleração da gravidade (m/s^2)
    L = 0.82        # Comprimento total do barbante (m)
    l = L/n         # distância entre dois pontos de discretização (m)
    T0 = mb*g       # Tração no ponto 0 (logo acima da bolinha) - considerando peso da bolinha (N)

    b = zeros(n)
    c = g/(2l)
    d = zeros(n)
    e = zeros(n)

    b[1] = g/l
    for k = 2:n
        b[k] = (T0 + ms*g*(k-1)*l)/(ms*l^2)
        d[k] = b[k] - c
        e[k] = b[k] + c
    end

	A = generateA(n, b, d, e, tau, taul)
	B = generateB(n,e[n])
	C = generateC(n)

	return A, B, C
end