class Moneda
	attr_reader :simbolo, :valor

	def en(atomo)
		self.cambio_moneda(atomo)
	end

	def comparar(cantidad)
		cantidad.comparacion(self)
	end
end

class Dolar < Moneda
	def initialize(valor)
		@simbolo = "$"
		@valor = valor
	end

	def cambio_moneda(atomo)
		case atomo
		when :yens
			conversion = self.valor * 106.38405 
			moneda = conversion.yens
		when :euros
			conversion = self.valor * 0.80870163
			moneda = conversion.euros
		when :bolivares
			conversion = self.valor *  217912.39
			moneda = conversion.bolivares
		when :bitcoins
			conversion = self.valor * 0.000120296
			moneda = conversion.bitcoins
		else
			puts "No existe esa moneda"
		end

		return moneda
	end


	def comparacion(cantidad)
		puts "Entre en dolar"
		case cantidad.simbolo
		when "¥"
			conversion = self.valor.dolares.en(:yens)
		when "€"
			conversion = self.valor.dolares.en(:euros)
		when "Bs"
			conversion = self.valor.dolares.en(:bolivares)
		when "BTC"
			conversion = self.valor.dolares.en(:bitcoins)
		else 
			puts "No se puede comparar"
		end

		if cantidad.valor < conversion.valor
			puts "menor"
		elsif cantidad.valor > conversion.valor
			puts "mayor"
		elsif cantidad.valor == conversion.valor
			puts "igual"
		end		
	end
end

class Yen < Moneda
	def initialize(valor)
		@simbolo = "¥"
		@valor = valor
	end

	def cambio_moneda(atomo)
		case atomo
		when :dolares
			conversion = self.valor * 0.00943
			moneda = conversion.dolares
		when :euros
			conversion = self.valor * 0.00764200102
			moneda = conversion.euros
		when :bolivares
			conversion = self.valor * 0.09436
			moneda = conversion.bolivares
		when :bitcoins
			conversion = self.valor * 0.00000113569 
			moneda = conversion.bitcoins
		else
			puts "No existe esa moneda"
		end

		return moneda
	end

	def comparacion(cantidad)
		case cantidad.simbolo
		when "$"
			conversion = self.valor.yens.en(:dolares)
		when "€"
			conversion = self.valor.yens.en(:euros)
		when "Bs"
			conversion = self.valor.yens.en(:bolivares)
		when "BTC"
			conversion = self.valor.yens.en(:bitcoins)
		else 
			puts "No se puede comparar"
		end

		if cantidad.valor < conversion.valor
			puts "menor"
		elsif cantidad.valor > conversion.valor
			puts "mayor"
		elsif cantidad.valor == conversion.valor
			puts "igual"
		end		
	end
end

class Euro < Moneda
	def initialize(valor)
		@simbolo = "€"
		@valor = valor
	end

	def cambio_moneda(atomo)
		case atomo
		when :yens
			conversion = self.valor * 130.855779
			moneda = conversion.yens
		when :dolares
			conversion = self.valor * 1.23397
			moneda = conversion.dolares
		when :bolivares
			conversion = self.valor * 269431.24
			moneda = conversion.bolivares
		when :bitcoins
			conversion = self.valor * 0.000148489 
			moneda = conversion.bitcoins
		else
			puts "No existe esa moneda"
		end

		return moneda
	end

	def comparacion(cantidad)
		case cantidad.simbolo
		when "¥"
			conversion = self.valor.euros.en(:yens)
		when "$"
			conversion = self.valor.euros.en(:dolares)
		when "Bs"
			conversion = self.valor.euros.en(:bolivares)
		when "BTC"
			conversion = self.valor.euros.en(:bitcoins)
		else 
			puts "No se puede comparar"
		end

		if cantidad.valor < conversion.valor
			puts "menor"
		elsif cantidad.valor > conversion.valor
			puts "mayor"
		elsif cantidad.valor == conversion.valor
			puts "igual"
		end		
	end
end

class Bolivar < Moneda
	def initialize(valor)
		@simbolo = "Bs"
		@valor = valor
	end

	def cambio_moneda(atomo)
		case atomo
		when :yens
			conversion = self.valor * 0.00318133616
			moneda = conversion.yens
		when :dolares
			conversion = self.valor * 0.00000463439
			moneda = conversion.dolares
		when :euros
			conversion = self.valor * 0.0000243117742
			moneda = conversion.euros
		when :bitcoins
			conversion = self.valor * 0.00000113618 
			moneda = conversion.bitcoins
		else
			puts "No existe esa moneda"
		end

		return moneda
	end

	def comparacion(cantidad)
		case cantidad.simbolo
		when "¥"
			conversion = self.valor.bolivares.en(:yens)
		when "€"
			conversion = self.valor.bolivares.en(:euros)
		when "$"
			conversion = self.valor.bolivares.en(:dolares)
		when "BTC"
			conversion = self.valor.bolivares.en(:bitcoins)
		else 
			puts "No se puede comparar"
		end

		if cantidad.valor < conversion.valor
			puts "menor"
		elsif cantidad.valor > conversion.valor
			puts "mayor"
		elsif cantidad.valor == conversion.valor
			puts "igual"
		end		
	end
end

class Bitcoin < Moneda
	def initialize(valor)
		@simbolo = "BTC"
		@valor = valor
	end

	def cambio_moneda(atomo)
		case atomo
			when :yens
				conversion = self.valor * 880143.94
				moneda = conversion.yens
			when :dolares
				conversion = self.valor * 8318.12 
				moneda = conversion.dolares
			when :euros
				conversion = self.valor * 6730.35
				moneda = conversion.euros
			when :bolivares
				conversion = self.valor * 1828518118.36
				moneda = conversion.bolivares
			else
				puts "No existe esa moneda"
			end

			return moneda
	end

	def comparacion(cantidad)
		case cantidad.simbolo
		when "¥"
			conversion = self.valor.bitcoins.en(:yens)
		when "€"
			conversion = self.valor.bitcoins.en(:euros)
		when "Bs"
			conversion = self.valor.bitcoins.en(:bolivares)
		when "$"
			conversion = self.valor.bitcoins.en(:dolares)
		else 
			puts "No se puede comparar"
		end

		if cantidad.valor < conversion.valor
			puts "menor"
		elsif cantidad.valor > conversion.valor
			puts "mayor"
		elsif cantidad.valor == conversion.valor
			puts "igual"
		end		
	end
end

class Numeric
	def dolares
		dolar = Dolar.new(self)
		puts "#{dolar.simbolo} #{dolar.valor}"
		return dolar
	end

	def yens
		yen = Yen.new(self)
		puts "#{yen.simbolo} #{yen.valor}"
		return yen
	end

	def euros
		euro = Euro.new(self)
		puts "#{euro.simbolo} #{euro.valor}"
		return euro
	end

	def bolivares
		bolivar = Bolivar.new(self)
		puts "#{bolivar.simbolo} #{bolivar.valor}"
		return bolivar
	end

	def bitcoins
		bitcoin = Bitcoin.new(self)
		puts "#{bitcoin.simbolo} #{bitcoin.valor}"
		return bitcoin
	end
end 


# 1.5.euros.en(:dolares)
# 1.bitcoins.en(:dolares)
# 800.dolares.en(:bitcoins)
# 8318.12.dolares.en(:bitcoins)
# (8318.12*2).dolares.en(:bitcoins)
# 1.24.dolares.en(:euros)
# 8318.12.dolares.comparar(1.bitcoins)
# 100000.bolivares.comparar(2.dolares)
# 1.23397.dolares.comparar(1.euros)