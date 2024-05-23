# -*- coding: utf-8 -*-

from __future__ import print_function
import time
from Model import Model
from PlantOPC import PlantOPC
import OpenOPC
import numpy
import matplotlib.pyplot as plt

opc = OpenOPC.client() # Cria o cliente OPC; o servidor é o RSLinx
opc.connect('RSLinx OPC Server') # Essa string não muda; conecta ao RSLinx

pC = numpy.array([0.5, 0.6, 0.7, 0.5 + 0.4j, 0.5 - 0.4j])
Ki = 0.183111320328469
Kp = numpy.array([0.007993734748865, 0.009705988539721, -0.004630469582507, -0.000426479250745])

t = numpy.array(range(0, n_t)) * Ts
# time = linspace(0,10,n_t)
# instantiate the plant that will be used, it should be a subclass of Plant

plant = PlantOPC(opc,'[CLP_AB]position','[CLP_AB]speed',init_pos)
model = Model(n, A, B, C, D, Ak, Bk, Ck, Q, R, Kp, Ki, epsilon, Ts, plant)

start = time.clock()
t_old = start
times_p = []
for i in range(0, n_t):
	y_out[i] = model.closed_loop(y_topo[i],y_fundo[i])
	#time.sleep(0.1)
plant.kill()
print("Total simulation time: {}s".format(time.clock() - start))

y_out_phased = y_out[5:n_t]
t_out_phased = t[0:n_t-5]
##plt.plot(t, y_out[0:n_t], label='out')
plt.plot(t_out_phased,y_out_phased, label='out_n')
plt.plot(t, y_fundo[0:n_t], label='ref fundo')
plt.plot(t, y_topo[0:n_t], label='ref topo (in)')
plt.legend(loc=4)
plt.xlabel('time (s)')
plt.ylabel('position (m)')
plt.title('Position of cart - close loop')
plt.grid(True)
# plt.show()
plt.savefig("resultados/closed_loop_trajetoria_rafael.png", format='png', dpi=200)
File = open('resultados/trajetoria_rafael.npz','wb')
numpy.savez(File, t=t, y_topo=y_topo, y_fundo=y_fundo, y_out=y_out, pC=pC, Ki=Ki, Kp=Kp)
File.close()

opc.close() # Encerra a sessão
