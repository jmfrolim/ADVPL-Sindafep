/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualiza��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! IMPCLI01 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importa��o do arquivo com dados de clintes				 !
!				   !  			 									 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 15/01/15 												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 									 		! 		 	! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/
#include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPCLI01	! Autor !Gilson Lima 		 ! Data ! 15/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importa��o do arquivo			 !
! 			! de Clientes													 !
+----------------------------------------------------------------------------*/
User Function IMPCLI01()

	Private cPerg	:= "IMPCLI01"
	Private aPergs	:= {}
	
	// Cria pergunta caso n�o exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Arquivo para Importa��o","Arquivo Importa��o","Arquivo Importa��o","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a fun��o de importa��o
	If MsgYesNo("Confirma importa��o do arquivo?","Aten��o")
		Processa({||IMPCLI02()},"Lendo Arquivo de Clientes...")
	EndIf
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPCLI02	! Autor !Gilson Lima 		 ! Data ! 15/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! L� os dados do arquivo			 							 !
! 			! de Clientes													 !
+----------------------------------------------------------------------------*/
Static Function IMPCLI02()

	Local nLin		:= 0
	Local cFile		:= ''
	Local cLinha	:= ''
	Local aClientes := {}
	Local aDados	:= {}
	Local cSepara	:= '|'
	Local nHdl		:= 0
	Local nTamFile	:= 0
	Local nLidos	:= 0
	Local nRestaLer	:= 0
	Local lContinua := .T.
	Local cEol      := CHR(13)+CHR(10)
	Local cLeitura	:= ''
	Local nFimLinha	:= 0
	Local nCount	:= 0
	Local nNrLinhas	:= 0
	Local cString	:= ''

	// Verifica se existe o arquivo texto
	cFile := AllTrim(MV_PAR01)
	
	// Abre o arquivo
	nHdl := fOpen(cFile,68)
	
	// Verifica se o arquivo p�de ser aberto, se n�o, encerra o programa
	If nHdl == -1
		MsgStop("O arquivo "+cFile+" n�o pode ser aberto... Verifique os par�metros","Atencao")
	����Return
	Endif	
	
	// Determina o tamanho do arquivo
	nTamFile := nRestaLer := fSeek(nHdl,0,2)
	
	// Posiciona no in�cio do arquivo
	fSeek(nHdl,0,0)
	
    ProcRegua(8800)
	
	While nRestaLer > 0 .And. lContinua
	
	    nCount++          
        IncProc("Lendo registro nr. " + cValToChar(nCount))
		
		// Posiciona o arquivo ap�s os dados lidos
		fSeek(nHdl,nLidos,0)
		
		// Vari�vel com o conte�do lido
		cLeitura� 	:= fReadStr(nHdl,65535)
		
		// Posi��o do final de linha
		nFimLinha 	:= AT(cEol,cLeitura)+1
		
		// Verifica se � a �ltima linha do arquivo
		If nFimLinha == 1
			cString	:= cLeitura
		Else
			cString	:= Substr(cLeitura,1,nFimLinha)
		EndIf
		
		nLidos		+= Len(cString)	
	����nRestaLer 	-= Len(cString)
	    
	    // Se n�o for �ltima linha
	   	If nFimLinha > 1
	    	cString	:= Substr(cString,1,Len(cString)-2)
	    EndIf
	    
	    // Gera Array com dados dos clientes
	    aDados   	:= StrToKarr(cString,AllTrim(cSepara))
	    
	    // Retira Espa�os
	    For nA := 1 To Len(aDados)
	    	aDados[nA] := AllTrim(aDados[nA])
	    Next nA
	    
	    // Adiciona ao Array de Clientes
	    aAdd(aClientes,aDados)
	
	End
	
	// Verifica se algum cliente foi lido do arquivo
	If Len(aClientes) > 0
		Processa({||IMPCLI03(aClientes)},"Gravando dados de Clientes...")		
	Else
		Alert("N�o foi importado nenhum registro.")
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPCLI03	! Autor !Gilson Lima 		 ! Data ! 15/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados do arquivo	de Clientes					 !
+----------------------------------------------------------------------------*/
Static Function IMPCLI03(aClientes)
	
	Local	nCount		:= 0
	Local	aCliente	:= {}
	Local   aErros		:= {}
	
	Local	nOpc		:= 3		// 3 = Inclus�o
	Local	cCodigo		:= ''
	Local	cLoja		:= '01'
	Local	cTipo		:= 'F'		// Consumidor Final

	Private	lMsErroAuto	:= .F.

	ProcRegua(Len(aClientes))
	
	For nA := 1 To Len(aClientes)
		
		nCount++
		IncProc("Gravando Cliente " + cValToChar(nCount))
		
		aCliente	:= {}
		lMsErroAuto	:= .F.
		
		//cCodigo	:= GetSXEnum("SA1","A1_COD")
		
		// Adiciona dados ao Array para execu��o do MSEXECAUTO
		aAdd(aCliente,{"A1_FILIAL"	,xFilial("SA1"),Nil})	  // Filial
		//aAdd(aCliente,{"A1_COD"		,cCodigo,Nil})			  // C�digo
		aAdd(aCliente,{"A1_LOJA"	,PadL(aClientes[nA][63],2,'0'),Nil})  // Loja
		aAdd(aCliente,{"A1_PESSOA"	,aClientes[nA][1] ,Nil})  // Fisica/Juridica
		aAdd(aCliente,{"A1_CODANT"	,aClientes[nA][2] ,Nil})  // Cod. Sistema Anterior
		aAdd(aCliente,{"A1_GRPVEN"	,PadL(aClientes[nA][3],6,'0'),Nil})  // Entidade
		aAdd(aCliente,{"A1_TIPO",cTipo,Nil})
		
		aAdd(aCliente,{"A1_NOME"	,Substr(Upper(aClientes[nA][4]),1,40),Nil})  // Nome
		
		If aClientes[nA][5] == 'NULL' .Or. AllTrim(aClientes[nA][5]) == ''
			nReduz	:= Substr(aClientes[nA][4],1,20)
		Else
			nReduz	:= Substr(aClientes[nA][5],1,20)
		EndIf
		aAdd(aCliente,{"A1_NREDUZ"	,Upper(nReduz) ,Nil})  // Nome Fantasia
		
		If aClientes[nA][6] == 'NULL' .Or. AllTrim(aClientes[nA][6]) == ''
			cEnd	:= 'NAO INFORMADO'
		Else
			cEnd	:= Substr(aClientes[nA][6],1,40)
		EndIf
		aAdd(aCliente,{"A1_END"		,Upper(cEnd) ,Nil})  // Endere�o

		If aClientes[nA][7] != 'NULL' .And. AllTrim(aClientes[nA][7]) != ''
			aAdd(aCliente,{"A1_COMPLEM"	,Substr(Upper(aClientes[nA][7]),1,50) ,Nil})  // Complemento
		EndIf

		If aClientes[nA][8] != 'NULL' .And. AllTrim(aClientes[nA][8]) != ''
			aAdd(aCliente,{"A1_BAIRRO"	,Substr(Upper(aClientes[nA][8]),1,30) ,Nil})  // Bairro
		EndIf

		If aClientes[nA][9] != 'NULL' .And. AllTrim(aClientes[nA][9]) != ''
			aAdd(aCliente,{"A1_CEP"		,PadR(aClientes[nA][9],8,'0') ,Nil})  // CEP
		EndIf

		If aClientes[nA][10] != 'NULL' .And. AllTrim(aClientes[nA][10]) != ''
			aAdd(aCliente,{"A1_MUN"		,Substr(Upper(aClientes[nA][10]),1,60),Nil})  // Munic�pio
		eLSE
			aAdd(aCliente,{"A1_MUN"		,Substr('NAO INFORMADO',1,60),Nil})  // Munic�pio
		EndIf

		If aClientes[nA][11] != 'NULL' .And. AllTrim(aClientes[nA][11]) != ''
			cUF := aClientes[nA][11]
		Else
			cUF := 'PR'
		EndIf
		aAdd(aCliente,{"A1_EST"		,Substr(Upper(cUF),1,2),Nil})  // Estado

		If aClientes[nA][12] != 'NULL' .And. AllTrim(aClientes[nA][12]) != ''
			If AT(")",aClientes[nA][12]) > 0
				aTel := StrToKarr(aClientes[nA][12],")")
				If Len(aTel) > 1 .And. Len(AllTrim(aTel[2])) > 7
					aTel[2] := StrTran(AllTrim(aTel[2])," ","")
					aTel[2] := StrTran(AllTrim(aTel[2]),".","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"-","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"_","")
					If Len(aTel[2]) < 8
						aTel[2] := PadL(aTel[2],8,'0')
					EndIf
					aTel[2] := PadL(aTel[2],9,' ')
					
					aTel[1] := StrTran(AllTrim(aTel[1]),'(',"")
					aTel[1] := Val(aTel[1])
					aTel[1] := PadL(cValtoChar(aTel[1]),3,'0')
					
					//cTel	:= "(" + aTel[1] + ")" + Substr(aTel[2],1,5) + "-" + Substr(aTel[2],6,4)
					// Campo A1_tel N�O ACEITA M�SCARA COM ESPA�OS E ()
					cTel	:= aTel[1]+aTel[2]
	
					aAdd(aCliente,{"A1_TEL"		,cTel,Nil})  // Tel. Residencial			
				EndIf
			EndIf
		EndIf

		If aClientes[nA][13] != 'NULL' .And. AllTrim(aClientes[nA][13]) != ''
			If AT(")",aClientes[nA][13]) > 0
				aTel := StrToKarr(aClientes[nA][13],")")
				If Len(aTel) > 1 .And. Len(AllTrim(aTel[2])) > 7
					aTel[2] := StrTran(AllTrim(aTel[2])," ","")
					aTel[2] := StrTran(AllTrim(aTel[2]),".","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"-","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"_","")
					If Len(aTel[2]) < 8
						aTel[2] := PadL(aTel[2],8,'0')
					EndIf
					aTel[2] := PadL(aTel[2],9,' ')
					
					aTel[1] := StrTran(AllTrim(aTel[1]),'(',"")
					aTel[1] := Val(aTel[1])
					aTel[1] := PadL(cValtoChar(aTel[1]),3,'0')
					
					cTel	:= "(" + aTel[1] + ")" + Substr(aTel[2],1,5) + "-" + Substr(aTel[2],6,4)
	
					aAdd(aCliente,{"A1_XCELUL"		,cTel,Nil})  // Tel. Celular			
				EndIf
			EndIf
		EndIf

		If aClientes[nA][14] != 'NULL' .And. AllTrim(aClientes[nA][14]) != ''
			If AT(")",aClientes[nA][14]) > 0
				aTel := StrToKarr(aClientes[nA][14],")")
				If Len(aTel) > 1 .And. Len(AllTrim(aTel[2])) > 7
					aTel[2] := StrTran(AllTrim(aTel[2])," ","")
					aTel[2] := StrTran(AllTrim(aTel[2]),".","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"-","")
					aTel[2] := StrTran(AllTrim(aTel[2]),"_","")
					If Len(aTel[2]) < 8
						aTel[2] := PadL(aTel[2],8,'0')
					EndIf
					aTel[2] := PadL(aTel[2],9,' ')
					
					aTel[1] := StrTran(AllTrim(aTel[1]),'(',"")
					aTel[1] := Val(aTel[1])
					aTel[1] := PadL(cValtoChar(aTel[1]),3,'0')
					
					cTel	:= "(" + aTel[1] + ")" + Substr(aTel[2],1,5) + "-" + Substr(aTel[2],6,4)
	
					aAdd(aCliente,{"A1_TELEX"		,cTel,Nil})  // Tel. Comercial			
				EndIf
			EndIf			
		EndIf
		
		cContCom := ''
		If aClientes[nA][15] != 'NULL' .And. AllTrim(aClientes[nA][15]) != ''
			cContCom += aClientes[nA][15]
		EndIf
		If aClientes[nA][67] != 'NULL' .And. AllTrim(aClientes[nA][67]) != ''
			If Len(cContCom) > 0
				cContCom += " | "
			EndIf
			cContCom += aClientes[nA][67]
		EndIf
		If Len(cContCom) > 0
			aAdd(aCliente,{"A1_XCNTCOM"	,Substr(Upper(cContCom),1,100),Nil})  // Contato Com
		EndIf
				
		If aClientes[nA][16] != 'NULL' .And. AllTrim(aClientes[nA][16]) != ''
			aAdd(aCliente,{"A1_XDESTCI"	,aClientes[nA][16],Nil})  // Estado Civil
		Else
			aAdd(aCliente,{"A1_XDESTCI"	,'7',Nil})  // Estado Civil - N�o Informado
		EndIF

		If aClientes[nA][17] != 'NULL' .And. AllTrim(aClientes[nA][17]) != ''
			aAdd(aCliente,{"A1_COJUGE"	,Substr(Upper(aClientes[nA][17]),1,60),Nil})  // Conjuge
		EndIf
		
		
		If aClientes[nA][1] == 'F'
			If aClientes[nA][18] != 'NULL' .And. AllTrim(aClientes[nA][18]) != ''
				If CGC(Substr(aClientes[nA][18],1,11))
					aAdd(aCliente,{"A1_CGC"		,Substr(aClientes[nA][18],1,11),Nil})  // CPF
				EndIf
				
			EndIf
		ElseIf aClientes[nA][1] == 'J'
			If aClientes[nA][19] != 'NULL' .And. AllTrim(aClientes[nA][19]) != ''
				If CGC(Substr(aClientes[nA][19],1,14))
					aAdd(aCliente,{"A1_CGC"		,Substr(aClientes[nA][19],1,14),Nil})  // CNPJ
				EndIf
			EndIf
		EndIf
		

		If aClientes[nA][20] != 'NULL' .And. AllTrim(aClientes[nA][20]) != ''
			aAdd(aCliente,{"A1_PFISICA"	,Substr(upper(aClientes[nA][20]),1,18),Nil})  // RG / IE
		EndIf

		If aClientes[nA][21] != 'NULL' .And. AllTrim(aClientes[nA][21]) != ''
			aAdd(aCliente,{"A1_XUFRG"	,Substr(Upper(aClientes[nA][21]),1,2),Nil})  // UF RG
		EndIf

		If aClientes[nA][22] != 'NULL' .And. AllTrim(aClientes[nA][22]) != ''
			aAdd(aCliente,{"A1_DTNASC"	,CtoD(TrataData(aClientes[nA][22])),Nil})  // Data de Nascimento
		EndIf

		If aClientes[nA][23] != 'NULL' .And. AllTrim(aClientes[nA][23]) != ''
			aAdd(aCliente,{"A1_EMAIL"	,Substr(Lower(aClientes[nA][23]),1,90),Nil})  // E-mail
		EndIf

		If aClientes[nA][24] != 'NULL' .And. AllTrim(aClientes[nA][24]) != ''
			aAdd(aCliente,{"A1_BLEMAIL"	,aClientes[nA][24],Nil})  // Boleto por Email
		EndIf

		If aClientes[nA][25] != 'NULL' .And. AllTrim(aClientes[nA][25]) != ''
			aAdd(aCliente,{"A1_SEXO"	,aClientes[nA][25],Nil})  // Sexo
		Else
			aAdd(aCliente,{"A1_SEXO"	,'M',Nil})  // Sexo
		EndIf

		If aClientes[nA][26] != 'NULL' .And. AllTrim(aClientes[nA][26]) != ''
			aAdd(aCliente,{"A1_NOMAE"	,Substr(Upper(aClientes[nA][26]),1,60),Nil})  // Nome da M�e
		Else
			aAdd(aCliente,{"A1_NOMAE"	,'NAO INFORMADO',Nil})  // Nome da M�e
		EndIf

		If aClientes[nA][27] != 'NULL' .And. AllTrim(aClientes[nA][27]) != ''
			aAdd(aCliente,{"A1_NOPAI"	,Substr(Upper(aClientes[nA][27]),1,60),Nil})  // Nome do Pai
		EndIf

		If aClientes[nA][28] != 'NULL' .And. AllTrim(aClientes[nA][28]) != ''
			aAdd(aCliente,{"A1_XDTFALEC",CtoD(TrataData(aClientes[nA][28])),Nil})  // Data de Falecimento
		EndIf

		cCjgFil := 'N'
		If aClientes[nA][29] == '1'
			cCjgFil	:= 'S'
		EndIf
		aAdd(aCliente,{"A1_CONJFI"	,cCjgFil,Nil})  // Conjuge Filiado (S/N)

		If aClientes[nA][30] != 'NULL' .And. AllTrim(aClientes[nA][30]) != ''
			aAdd(aCliente,{"A1_XSITDFI"	,aClientes[nA][30],Nil})  // Situa��o Fiscal
		Else
			aAdd(aCliente,{"A1_XSITDFI"	,'XX',Nil})  // Situa��o Fiscal - N�o Fiscal
		EndIf

		If aClientes[nA][31] != 'NULL' .And. AllTrim(aClientes[nA][31]) != ''
			aAdd(aCliente,{"A1_XFILNSI"	,aClientes[nA][31],Nil})  // Filiado Sindicado?
		Else
			aAdd(aCliente,{"A1_XFILNSI"	,'N',Nil})  // Filiado Sindicado? - N�o
		EndIf

		If aClientes[nA][32] != 'NULL' .And. AllTrim(aClientes[nA][32]) != ''
			aAdd(aCliente,{"A1_XDTFIL"	,CtoD(TrataData(aClientes[nA][32])),Nil})  // Data de Filia��o
		EndIf

		If aClientes[nA][33] != 'NULL' .And. AllTrim(aClientes[nA][33]) != ''
			aAdd(aCliente,{"A1_XDTDESF"	,CtoD(TrataData(aClientes[nA][33])),Nil})  // Data de Desfilia��o
		EndIf
		
		// Tratar
		If aClientes[nA][34] != 'NULL' .And. AllTrim(aClientes[nA][34]) != ''
			aAdd(aCliente,{"A1_XDEPART"	,PadL(aClientes[nA][34],6,'0'),Nil})  // C�digo Delegacia
		EndIf

		// Tratar
		If aClientes[nA][35] != 'NULL' .And. AllTrim(aClientes[nA][35]) != ''
			aAdd(aCliente,{"A1_XCARGO"	,PadL(aClientes[nA][35],6,'0'),Nil})  // C�digo Cargo
		EndIf
		
		If aClientes[nA][36] != 'NULL' .And. AllTrim(aClientes[nA][36]) != ''
			aAdd(aCliente,{"A1_XDTNOME"	,CtoD(TrataData(aClientes[nA][36])),Nil})  // Data de Nomea��o
		EndIf
		
		If aClientes[nA][37] != 'NULL' .And. AllTrim(aClientes[nA][37]) != ''
			aAdd(aCliente,{"A1_XAPOSE"	,aClientes[nA][37],Nil})  // Cod. Aposentadoria
		EndIf
		
		If aClientes[nA][38] != 'NULL' .And. AllTrim(aClientes[nA][38]) != ''
			aAdd(aCliente,{"A1_XDTAPOS"	,CtoD(TrataData(aClientes[nA][38])),Nil})  // Data Aposentadoria
		EndIf

		If aClientes[nA][39] != 'NULL' .And. AllTrim(aClientes[nA][39]) != ''
			aAdd(aCliente,{"A1_XPERCAP"	,Val(aClientes[nA][39]),Nil})  // % Aposentadoria
		EndIf
		
		If aClientes[nA][40] != 'NULL' .And. AllTrim(aClientes[nA][40]) != ''
			aAdd(aCliente,{"A1_XMESRFS"	,aClientes[nA][40],Nil})  // M�s Ref. Sal�rio
		EndIf
		
		If aClientes[nA][41] != 'NULL' .And. AllTrim(aClientes[nA][41]) != ''
			aAdd(aCliente,{"A1_SALARIO"	,Val(aClientes[nA][41]),Nil})  // Sal�rio
		EndIf

		If aClientes[nA][42] != 'NULL' .And. AllTrim(aClientes[nA][42]) != ''
			aAdd(aCliente,{"A1_VLRMENS"	,Val(aClientes[nA][42]),Nil})  // Valor Mensalidade
		EndIf
		
		If aClientes[nA][43] != 'NULL' .And. AllTrim(aClientes[nA][43]) != ''
			aAdd(aCliente,{"A1_DTDEBCO"	,CtoD(TrataData(aClientes[nA][43])),Nil})  // Data D�b. Conta
		EndIf
		
		If aClientes[nA][44] != 'NULL' .And. AllTrim(aClientes[nA][44]) != ''
			aAdd(aCliente,{"A1_DTDEBFO"	,CtoD(TrataData(aClientes[nA][44])),Nil})  // Data D�b. Folha
		EndIf
		
		If aClientes[nA][45] != 'NULL' .And. AllTrim(aClientes[nA][45]) != ''
			aAdd(aCliente,{"A1_FORMCOB"	,aClientes[nA][45],Nil})  // Forma Cobran�a
		EndIf
		
		If aClientes[nA][46] != 'NULL' .And. AllTrim(aClientes[nA][46]) != ''
			aAdd(aCliente,{"A1_XBANCO"	,aClientes[nA][46],Nil})  // Banco
		EndIf
		
		If aClientes[nA][47] != 'NULL' .And. AllTrim(aClientes[nA][47]) != ''
			aAdd(aCliente,{"A1_XAGENCI"	,aClientes[nA][47],Nil})  // Ag�ncia
		EndIf
		
		If aClientes[nA][48] != 'NULL' .And. AllTrim(aClientes[nA][48]) != ''
			aAdd(aCliente,{"A1_XCONTA"	,aClientes[nA][48],Nil})  // Nr. Conta
		EndIf
		
		If aClientes[nA][49] != 'NULL' .And. AllTrim(aClientes[nA][49]) != ''
			aAdd(aCliente,{"A1_XCTACPF"	,aClientes[nA][49],Nil})  // CPF Conta
		EndIf
		
		If aClientes[nA][50] != 'NULL' .And. AllTrim(aClientes[nA][50]) != ''
			aAdd(aCliente,{"A1_XOBS"	,Substr(Upper(aClientes[nA][50]),1,200),Nil})  // Observa��o Conta
		EndIf
		
		If aClientes[nA][51] != 'NULL' .And. AllTrim(aClientes[nA][51]) != ''
			aAdd(aCliente,{"A1_XCONTOP"	,aClientes[nA][51],Nil})  // Opera��o Conta
		EndIf
		
		If aClientes[nA][52] != 'NULL' .And. AllTrim(aClientes[nA][52]) != ''
			aAdd(aCliente,{"A1_XBCOUSR"	,aClientes[nA][52],Nil})  // Banco Associado
		EndIf
		
		If aClientes[nA][53] != 'NULL' .And. AllTrim(aClientes[nA][53]) != ''
			aAdd(aCliente,{"A1_XTITBCO"	,Substr(Upper(aClientes[nA][53]),1,50),Nil})  // Titular da Conta
		EndIf
		
		If aClientes[nA][54] != 'NULL' .And. AllTrim(aClientes[nA][54]) != ''
			aAdd(aCliente,{"A1_XDTINIP"	,CtoD(TrataData(aClientes[nA][54])),Nil})  // Dt. Inicio Pens�o
		EndIf
		
		If aClientes[nA][55] != 'NULL' .And. AllTrim(aClientes[nA][55]) != ''
			aAdd(aCliente,{"A1_XPERCPE"	,Val(aClientes[nA][55]),Nil})  // % Pens�o
		EndIf
		
		If aClientes[nA][56] != 'NULL' .And. AllTrim(aClientes[nA][56]) != ''
			aAdd(aCliente,{"A1_GERAPE"	,Substr(Upper(aClientes[nA][56]),1,60),Nil})  // Gerador Pens�o
		EndIf

		If aClientes[nA][57] != 'NULL' .And. AllTrim(aClientes[nA][57]) != ''
			aAdd(aCliente,{"A1_SITUACA"	,aClientes[nA][57],Nil})  // Situa��o do Cotista
		EndIf
		
		If aClientes[nA][58] != 'NULL' .And. AllTrim(aClientes[nA][58]) != ''
			aAdd(aCliente,{"A1_TITULO"	,aClientes[nA][58],Nil})  // Titulo
		EndIf
		
		If aClientes[nA][59] != 'NULL' .And. AllTrim(aClientes[nA][59]) != ''
			aAdd(aCliente,{"A1_SERIE"	,aClientes[nA][59],Nil})  // Serie
		EndIf
		
		If aClientes[nA][60] != 'NULL' .And. AllTrim(aClientes[nA][60]) != ''
			aAdd(aCliente,{"A1_MATPAG"	,aClientes[nA][60],Nil})  // Pag. Manut. Ano
		EndIf
		
		If aClientes[nA][61] != 'NULL' .And. AllTrim(aClientes[nA][61]) != ''
			aAdd(aCliente,{"A1_VLRANUI"	,Val(aClientes[nA][61]),Nil})  // Valor Anuidade
		EndIf
		
		If aClientes[nA][62] != 'NULL' .And. AllTrim(aClientes[nA][62]) != ''
			aAdd(aCliente,{"A1_COTOBS"	,Substr(Upper(aClientes[nA][62]),1,200),Nil})  // Observa��o Cotista
		EndIf
		
		If aClientes[nA][64] != 'NULL' .And. AllTrim(aClientes[nA][64]) != ''
			aAdd(aCliente,{"A1_XNCOTAS"	,Val(aClientes[nA][64]),Nil})  // N�mero de Cotas
		EndIf
		
		If aClientes[nA][65] != 'NULL' .And. AllTrim(aClientes[nA][65]) != ''
			If (IE(aClientes[nA][65],cUF))
				aAdd(aCliente,{"A1_INSCR"	,aClientes[nA][65],Nil})  // Inscri��o Estadual
			EndIf
		EndIf

		If aClientes[nA][66] != 'NULL' .And. AllTrim(aClientes[nA][66]) != ''
			aAdd(aCliente,{"A1_INSCRM"	,aClientes[nA][66],Nil})  // Inscri��o Municipal
		EndIf

		// Executa a Inclus�o		
		MSExecAuto({|x,y| Mata030(x,y)},aCliente,nOpc) //3- Inclus�o, 4- Altera��o, 5- Exclus�o
		
		// Verifica por Erro
		
		If lMsErroAuto
			//aAdd(aErros,{cValtoChar(nCount),aClientes[nA][2]})
			MostraErro()
			//Return()
		//Else
			//ConfirmSX8()
		EndIf
		
	Next
	
	/*
	If Len(aErros) > 0
		
		cArquivo	 := 'C:/Temp/ErrosImportacao.txt'
		nHdl2		 := fCreate(cArquivo)	// Inicia grava��o do arquivo
		
		// Caso n�o consiga criar arquivo, informa o erro e aborta o sistema
		If nHdl2 == -1
			MsgAlert("O arquivo de nome "+cArquivo+" n�o pode ser criado! Verifique os par�metros.","Aten��o!")
			Return
		EndIf
		
		cLinha := ''
		For nB :=1 To Len(aErros)
			
			cLinha += PadL(aErros[nB][1],9,' ') + " | " + PadL(aErros[nB][2],9,' ')  
			cLinha += CRLF
			
			fWrite(nHdl2,cLinha,Len(cLinha))
		
		Next nB
		
		fClose(nHdl2)
		
	EndIf
	*/
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! TRATADATA	! Autor !Gilson Lima 		 ! Data ! 16/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Converte data no formato MM/DD/AAAA para DD/MM/AAAA			 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function TrataData(cData)

	Local cNData := ''
	
	cNData := Substr(cData,4,2) + "/" + Substr(cData,1,2) + "/" + Substr(cData,7,4)

Return cNData