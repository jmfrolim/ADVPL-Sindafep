/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FAT                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! RRECIB000                                               !
+------------------+---------------------------------------------------------+
!Descricao         ! Recibo de Titulos. 									 !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Gilson M. Lima				                             !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 26/01/2015                                              !
+------------------+---------------------------------------------------------+
!   ATUALIZACOES                                                             !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!                                           !           !           !        !
!                                           !           !           !        !
+-------------------------------------------+-----------+-----------+--------+
*/
#INCLUDE "PROTHEUS.CH"

#define DMPAPER_A4 9

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RRECIB000	! Autor !Gilson Lima 		 ! Data ! 26/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a geração do recibo				 !
! 			!  																 !
+----------------------------------------------------------------------------*/
User Function RRECIB000(nRecOpc,nRecno)
	Local cChave	:= SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA
	Private cPerg		:= "RRECIB"
	Private aPergs		:= {}
	Private nOpcao		:= nRecOpc
	
	If nOpcao == 1 // Inclusão
		// Validacao antes de emitir o recibo para evitar a criacao de novos recibos para titulos recibos ja emitidos.
		aArea := GetArea()
		
		dbSelectArea("SZR")
		SZR->(dbSetOrder(2))
		SZR->(dbSeek(xFilial("SZR")+cChave))
		
		While SZR->(!EOF()) .And. SZR->(ZR_TITPREF+ZR_TITNUM+ZR_TITPARC) == cChave
			If SZR->ZR_STATUS != '2'
				Aviso("Atencao", "O recibo ja foi emitido para este titulo." + CHR(13) + CHR(10) + "Para visualizar o recibo, feche e abra a tela do titulo novamente.", {"Ok"}, 2)
				Return
			EndIf
			
			SZR->(dbSkip())
		End
		
		SZR->(dbCloseArea())
		RestArea(aArea)
		
		// Cria pergunta caso não exista
		SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
		If SX1->(!Found())
	
			//AADD(aPergs,{"Série","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","A","","","","","B","","","","","C","","","","","D","","","","","","","","","","","","",""})
			AADD(aPergs,{"Série","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","A","","","","","C","","","","","H","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Forma de Pagamento","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","Cheque","","","","","Depósito","","","","","Dinheiro","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Banco","","","mv_ch3","C",4,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Cheque","","","mv_ch4","C",10,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	
			AADD(aPergs,{"Descrição","","","mv_ch5","C",60,0,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Descrição","","","mv_ch6","C",60,0,0,"G","","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Descrição","","","mv_ch7","C",60,0,0,"G","","MV_PAR07","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Descrição","","","mv_ch8","C",60,0,0,"G","","MV_PAR08","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AADD(aPergs,{"Descrição","","","mv_ch9","C",60,0,0,"G","","MV_PAR09","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
			AjustaSx1(cPerg,aPergs)
		EndIf
		
		// Carrega Perguntas
		Pergunte(cPerg,.T.)
	
	EndIf
	
	If nOpcao == 3 // Exclusao
		
		If MsgYesNo("Confirma Exclusão do Recibo?","Atenção!")
			Processa({|| ExcluiRec()},"Excluindo Recibo")
		EndIf
		
		Return
		
	EndIf
	
	Processa({|| RRecib001(nRecno)},"Gerando Recibo")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RRECIB001	! Autor !Gilson Lima 		 ! Data ! 26/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao tratamento de dados e chamada de recibo				 !
! 			!  																 !
+----------------------------------------------------------------------------*/
Static Function RRECIB001(nRecno)

	Local oPrint := Nil
	Local aArea
	Local cChave	

	Private nSerie
	Private nFPagto
	Private cBanco
	Private cCheque

	Private cDescr1
	Private cDescr2
	Private cDescr3
	Private cDescr4
	Private cDescr5
	
	Private cSerRec
	Private cFPagto
	Private aMensagem := Array(3)

	Private cNrRec
	Private cTitTipo
	Private cTitHist
	Private cTitVenc
	Private cTitVal
	Private cNaturez
	Private cCliente
	Private cGrpVen
	Private cGrpVenD
	Private cNome
	Private cEnd
	Private cCompl
	Private cBairro
	Private cCEP
	Private cCidade
	Private cUF
	Private cTel1
	Private cTel2
	Private cTel	
	Private cMensCanc := ''
	
	Private cEnder
	Private cCepCidUF
	
	Private cEmissor
	Private cDtEmissao	
	
	If nOpcao == 1 // Inclusão
	
		nSerie  := MV_PAR01
		nFPagto := MV_PAR02
		cBanco	:= MV_PAR03
		cCheque	:= MV_PAR04
	
		cDescr1	:= MV_PAR05
		cDescr2	:= MV_PAR06
		cDescr3	:= MV_PAR07
		cDescr4	:= MV_PAR08
		cDescr5	:= MV_PAR09
		
		cSerRec	:= "A"
		cFPagto	:= ''
		
		Do Case
			Case nFPagto == 1
				cFPagto 	 := 'CHEQUE'
				aMensagem[1] := 'Este documento não comprova quitação financeira.'
				aMensagem[2] := 'A quitação do título somente ocorrerá após a'
				aMensagem[3] := 'compensação do cheque.'
			Case nFPagto == 2
				cFPagto 	 := 'DEPÓSITO'
				aMensagem[1] := 'Este documento não comprova quitação financeira.'
				aMensagem[2] := 'A quitação do título somente ocorrerá após a'
				aMensagem[3] := 'identificação do depósito em conta.'
				cBanco		 := ''
				cCheque		 := ''
			Case nFPagto == 3
				cFPagto 	 := 'DINHEIRO'
				aMensagem[1] := ''
				aMensagem[2] := ''
				aMensagem[3] := 'Este documento não comprova quitação financeira.'
				cBanco		 := ''
				cCheque		 := ''
		EndCase
		
		/*
		Do Case
			Case nSerie == 1
				cSerRec := 'A'
			Case nSerie == 2
				cSerRec := 'B'
			Case nSerie == 3
				cSerRec := 'C'
			Case nSerie == 4
				cSerRec := 'D'
			Otherwise
				cSerRec	:= 'A'
		EndCase
		*/
		
		Do Case
			Case nSerie == 1
				cSerRec := 'A'
			Case nSerie == 2
				cSerRec := 'C'
			Case nSerie == 3
				cSerRec := 'H'
			Otherwise
				cSerRec	:= 'A'
		EndCase
		
		cNrRec	 := GeraNr(cSerRec)
		cTitTipo := SE1->E1_TIPO
		cTitHist := Alltrim(cTitTipo) + ' ' + AllTrim(SE1->E1_PREFIXO) + " " + AllTrim(SE1->E1_NUM) + " " + AllTrim(SE1->E1_PARCELA) + " | " + Substr(AllTrim(SE1->E1_HIST),1,46)
		cTitVenc := DtoC(SE1->E1_VENCREA)
		cTitVal	 := Transform(SE1->E1_VALOR,'@E 999,999,999.99')
		cNaturez := AllTrim(Posicione("SED",1,xFilial("SED")+SE1->E1_NATUREZ,"ED_DESCRIC"))
		cCliente := Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_COD")
		cGrpVen	 := Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_GRPVEN")
		cGrpVenD := AllTrim(Posicione("ACY",1,xFilial("ACY")+cGrpVen,"ACY_DESCRI"))
		cNome	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_NOME"))
		cEnd	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_END"))
		cCompl	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_COMPLEM"))
		cBairro	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_BAIRRO"))
		cCEP	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_CEP"))
		cCidade	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_MUN"))
		cUF	 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_EST"))
		cTel1 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_TEL"))
		cTel2 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_XCELUL"))
		cTel	 := ''
		
		cEmissor   := CUSERNAME
		cDtEmissao := DtoC(dDataBase) 	
		
		
		If AllTrim(cTel1) != ''
			cTel += cTel1
		EndIf
		If AllTrim(cTel2) != ''
		 	If Alltrim(cTel) != ''
		 		cTel += " | "
		 	EndIf
		 	cTel += cTel2
		EndIf
		
		cEnder	 := cEnd
		If AllTrim(cCompl) != ''
			cEnder += " - " + cCompl
		EndIf
		If AllTrim(cBairro) != ''
			cEnder += " - " + cBairro
		EndIf
		
		cCepCidUF 	  := ''
		If AllTrim(cCep) != ''
			cCepCidUF += Substr(cCEP,1,5) + "-" + Substr(cCep,6,3)
		EndIf
		If AllTrim(cCidade) != ''
			If AllTrim(cCepCidUf)!= ''
				cCepCidUF += ' - '
			EndIf
			cCepCidUF += cCidade
		EndIf
		If AllTrim(cUf) != ''
			If AllTrim(cCepCidUf)!= ''
				cCepCidUF += " / "
			EndIf
			cCepCidUf += cUf
		EndIf
		
		// Grava dados do recibo na SZR
		GravaRecib()
	
	EndIf
	
	If nOpcao == 2 // Visualização
		
		aArea 	:= GetArea()
		cChave	:= SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA
		
		dbSelectArea("SZR")
		
		If nRecno == Nil
			SZR->(dbSetOrder(2))
			SZR->(dbSeek(xFilial("SZR")+cChave))
			While SZR->(!EOF()) .And. SZR->(ZR_TITPREF+ZR_TITNUM+ZR_TITPARC) == cChave
				
				nRecno := SZR->(Recno())
				
				SZR->(dbSkip())
			End		
		EndIF
		
		SZR->(dbGoTo(nRecno))
			
		cNrRec		:= AllTrim(SZR->ZR_NUM)	
		cSerRec		:= AllTrim(SZR->ZR_SERIE)
		cFPagto		:= AllTrim(SZR->ZR_FPAGTO)
	
		cBanco		:= AllTrim(SZR->ZR_BANCO)
		cCheque		:= AllTrim(SZR->ZR_CHEQUE)

		cDescr1		:= AllTrim(SZR->ZR_DESCR1)
		cDescr2		:= AllTrim(SZR->ZR_DESCR2)
		cDescr3		:= AllTrim(SZR->ZR_DESCR3)
		cDescr4		:= AllTrim(SZR->ZR_DESCR4)
		cDescr5		:= AllTrim(SZR->ZR_DESCR5)
		
		cTitTipo	:= SZR->ZR_TIPO
		cTitHist	:= Alltrim(cTitTipo) + ' ' + AllTrim(SZR->ZR_TITPREF) + ' ' + AllTrim(SZR->ZR_TITNUM) + " " + AllTrim(SZR->ZR_TITPARC) + " | " + Substr(AllTrim(SZR->ZR_TITDESC),1,46)
		cTitVenc	:= DtoC(SZR->ZR_TITDTV)
		cTitVal		:= Transform(SZR->ZR_TITVL,'@E 999,999,999.99')
		cNaturez	:= AllTrim(SZR->ZR_TITNAT)

		cCliente	:= AllTrim(SZR->ZR_CLICOD)
		cNome		:= AllTrim(SZR->ZR_CLINOME)
		cGrpVenD	:= AllTrim(SZR->ZR_CLIGRP)
		cEnder		:= AllTrim(SZR->ZR_CLIEND)
		cCepCidUF	:= AllTrim(SZR->ZR_CLICIUF)
		cTel		:= AllTrim(SZR->ZR_CLITEL)
		
		cEmissor	:= AllTrim(SZR->ZR_EMISSOR)
		cDtEmissao	:= DtoC(SZR->ZR_DTEMISS)
		
		If SZR->ZR_STATUS == '2'
			cMensCanc := "RECIBO CANCELADO"
		EndIf

		Do Case
			Case cFPagto == 'CHEQUE'
				aMensagem[1] := 'Este documento não comprova quitação financeira.'
				aMensagem[2] := 'A quitação do título somente ocorrerá após a'
				aMensagem[3] := 'compensação do cheque.'
			Case cFPagto == 'DEPÓSITO'
				aMensagem[1] := 'Este documento não comprova quitação financeira.'
				aMensagem[2] := 'A quitação do título somente ocorrerá após a'
				aMensagem[3] := 'identificação do depósito em conta.'
			Case cFPagto == 'DINHEIRO'
				aMensagem[1] := ''
				aMensagem[2] := ''
				aMensagem[3] := 'Este documento não comprova quitação financeira.'
		EndCase
			
		SZR->(dbCloseArea())	
	
		RestARea(aArea)		
		
	EndIf
	
	oPrint:= TMSPrinter():New("Recibo Sindafep")
	oPrint:Setup()
	oPrint:SetPaperSize(9)
	oPrint:SetPortrait()
	oPrint:StartPage()	

	GeraRecib(@oPrint,70)
	GeraRecib(@oPrint,1170)
	GeraRecib(@oPrint,2280)
	
	oPrint:EndPage()
	oPrint:Preview()
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERARECIB	! Autor !Gilson Lima 		 ! Data ! 26/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao Geração do Layout do Recibo							 !
! 			!  																 !
+----------------------------------------------------------------------------*/
Static Function GeraRecib(oPrint,nRow)
	
	Local cLogo :='\system\sindafep.bmp'
	
	Private oFont8		:= TFont():New("Arial",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8b		:= TFont():New("Arial",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont8m		:= TFont():New("Courier New",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8mb	:= TFont():New("Courier New",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9		:= TFont():New("Arial",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9b		:= TFont():New("Arial",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9m		:= TFont():New("Courier New",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9mb	:= TFont():New("Courier New",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10		:= TFont():New("Arial",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10b	:= TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10m	:= TFont():New("Courier New",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10mb	:= TFont():New("Courier New",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12		:= TFont():New("Arial",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12b	:= TFont():New("Arial",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12m	:= TFont():New("Courier New",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12mb	:= TFont():New("Courier New",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14		:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14b	:= TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14m	:= TFont():New("Courier New",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14mb	:= TFont():New("Courier New",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont16b	:= TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18b	:= TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18mb	:= TFont():New("Courier New",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont20mb	:= TFont():New("Courier New",9,20,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont24b	:= TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	If (File(cLogo))
		oPrint:SayBitmap(nRow+35,100,cLogo,310,200)
	EndIf

	oPrint:Say(nRow+30,480,"SINDAFEP - Sindicato dos Auditores Fiscais da Receita do Estado do Paraná",oFont10b)
	
	oPrint:Say(nRow+100,480,"SEDE: Rua Alferes Angelo Sampaio, 1793, Batel - Curitiba/PR - CEP 80420-160",oFont8)
	oPrint:Say(nRow+100,1700,"(41) 3221-5300",oFont8)
	
	oPrint:Say(nRow+140,480,"COLÔNIA DE FÉRIAS: Rua Tibagi, 77 - Guaratuba/PR - CEP 83280-000",oFont8)
	oPrint:Say(nRow+140,1700,"(41) 3442-1585",oFont8)
	
	oPrint:Say(nRow+180,480,"HOTAL ROTA DO SOL: Av. Visconde do Rio Branco, 2995 - Guaratuba/PR - CEP 83280-000",oFont8)
	oPrint:Say(nRow+180,1700,"(41) 3443-1313",oFont8)
	
	oPrint:Say(nRow+230,480,"CNPJ: 76.707.686/0001-17",oFont8)
	oPrint:Say(nRow+230,1545,"SITE: www.sindafep.com.br",oFont8)
	
	oPrint:Box(nRow+30,2000,nRow+320,2300)
	
	oPrint:Say(nRow+60,2055,'Série "' + cSerRec + '"',oFont12b)
	oPrint:Say(nRow+140,2056,'RECIBO',oFont12b)
	oPrint:Say(nRow+220,2030,PadL(cNrRec,7,' '),oFont14mb)
	
	oPrint:Box(nRow+320,100,nRow+550,2300)
	
	oPrint:Say(nRow+340,120,'Sócio:',oFont9)
	oPrint:Say(nRow+410,120,'Nome:',oFont9)
	oPrint:Say(nRow+480,120,'Endereço:',oFont9)

	oPrint:Say(nRow+340,300,cGrpVenD,oFont9)
	oPrint:Say(nRow+410,300,cNome,oFont9)
	oPrint:Say(nRow+480,300,cEnder,oFont9)

	oPrint:Say(nRow+340,1450,'Identificação:',oFont9)
	oPrint:Say(nRow+410,1450,'Telefone(s):',oFont9)
	oPrint:Say(nRow+480,1450,'CEP - Cidade/UF:',oFont9)
	
	oPrint:Say(nRow+340,1750,cCliente,oFont9)
	oPrint:Say(nRow+410,1750,cTel,oFont9)
	oPrint:Say(nRow+480,1750,cCepCidUF,oFont9)
	
	oPrint:Say(nRow+575,120,'NATUREZA DA OPERAÇÃO:',oFont9)
	oPrint:Say(nRow+578,585,cNaturez,oFont9m)
	
	If nFPagto == 1 // Cheque
		oPrint:Say(nRow+575,1390,'BANCO',oFont9)
		oPrint:Say(nRow+575,1550,'CHEQUE NR.',oFont9)
	Else
		oPrint:Say(nRow+575,1390,cFPagto,oFont9)
	EndIf
	
	oPrint:Say(nRow+575,1810,'DT. VENCIM.',oFont9)
	oPrint:Say(nRow+575,2100,'VALOR R$',oFont9)

	oPrint:Box(nRow+640,100,nRow+1000,2300)
	
	oPrint:Say(nRow+670,120,cTitHist,oFont9m)
	
	If nFPagto == 1 // Cheque
		oPrint:Say(nRow+670,1420,cBanco,oFont9m)
		oPrint:Say(nRow+670,1560,cCheque,oFont9m)
	EndIf
	
	oPrint:Say(nRow+670,1810,cTitVenc,oFont9m)
	oPrint:Say(nRow+670,1985,cTitVal,oFont9m)
	
	// Mensagem de Recibo Cancelado
	oPrint:Say(nRow+780,1390,cMensCanc,oFont12b)
	
	oPrint:Say(nRow+835,1390,aMensagem[1],oFont9b)
	oPrint:Say(nRow+890,1390,aMensagem[2],oFont9b)
	oPrint:Say(nRow+945,1390,aMensagem[3],oFont9b)
	
	oPrint:Say(nRow+725,120,cDescr1,oFont9m)
	oPrint:Say(nRow+780,120,cDescr2,oFont9m)
	oPrint:Say(nRow+835,120,cDescr3,oFont9m)
	oPrint:Say(nRow+890,120,cDescr4,oFont9m)
	oPrint:Say(nRow+945,120,cDescr5,oFont9m)
	
	oPrint:Box(nRow+1000,100,nRow+1090,2300)
	
	oPrint:Say(nRow+1025,120,'Data Emissão:',oFont9)
	oPrint:Say(nRow+1025,350,cDtEmissao,oFont9)
	
	oPrint:Line(nRow+1000,750,nRow+1090,750)
	
	oPrint:Say(nRow+1025,800,cEmissor,oFont9)

	oPrint:Line(nRow+1000,1750,nRow+1090,1750)
	
	oPrint:Say(nRow+1025,1800,'TOTAL',oFont9)
	oPrint:Say(nRow+1025,1985,cTitVal,oFont9mb)
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERANR	! Autor !Gilson Lima 		 ! Data ! 26/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao Geração / Gravação Nr para o recibo baseado na Série	 !
! 			!  																 !
+----------------------------------------------------------------------------*/
Static Function GeraNr(cSerie)
	Local aArea		:= GetArea()
	Local cTabela 	:= 'RN'
	Local cChave  	:= PadR(cSerie,6,' ')
	Local cNum		 := ''
	
	cNum := Posicione("SX5",1,xFilial("SX5")+cTabela+cChave,"X5_DESCRI")
	cNum := cValToChar(Val(cNum))
	
	dbSelectArea("SX5")
	SX5->(dbSeek(xFilial("SX5")+cTabela+cChave))
	
	RecLock("SX5",.F.)
		SX5->X5_DESCRI := PadL(Val(cNum)+1,6,'0')
	SX5->(MsUnlock())
	
	SX5->(dbCloseArea())
	RestArea(aArea)
Return(cNum)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRAVARECIB! Autor !Gilson Lima 		 ! Data ! 28/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao de Gravação dos dados do recibo na tabela SZR			 !
! 			!  																 !
+----------------------------------------------------------------------------*/
Static Function GravaRecib()

	Local aArea := GetArea()
	
	dbSelectArea("SZR")
	
		RecLock("SZR",.T.)
		
			SZR->ZR_FILIAL	:= xFilial("SZR")
			SZR->ZR_NUM		:= cNrRec
			SZR->ZR_SERIE	:= cSerRec
			SZR->ZR_FPAGTO	:= cFPagto
			SZR->ZR_BANCO	:= cBanco
			SZR->ZR_CHEQUE	:= cCheque
			SZR->ZR_DESCR1	:= cDescr1
			SZR->ZR_DESCR2	:= cDescr2
			SZR->ZR_DESCR3	:= cDescr3
			SZR->ZR_DESCR4	:= cDescr4
			SZR->ZR_DESCR5	:= cDescr5
			SZR->ZR_DTEMISS	:= dDataBase
			SZR->ZR_EMISSOR	:= CUSERNAME
			SZR->ZR_TIPO	:= cTitTipo
			SZR->ZR_TITPREF	:= SE1->E1_PREFIXO
			SZR->ZR_TITNUM	:= SE1->E1_NUM
			SZR->ZR_TITPARC	:= SE1->E1_PARCELA
			SZR->ZR_TITNAT	:= cNaturez
			SZR->ZR_TITDESC	:= AllTrim(SE1->E1_HIST)
			SZR->ZR_TITDTV	:= SE1->E1_VENCREA
			SZR->ZR_TITVL	:= SE1->E1_VALOR
			SZR->ZR_CLICOD	:= cCliente
			SZR->ZR_CLIGRP	:= cGrpVenD
			SZR->ZR_CLINOME	:= cNome
			SZR->ZR_CLIEND	:= cEnder
			SZR->ZR_CLICIUF	:= cCepCidUf
			SZR->ZR_CLITEL	:= cTel
			SZR->ZR_STATUS	:= '1'	// aTIVO
		
		SZR->(MsUnLock())
	
	SZR->(dbCloseArea())
	
	RestArea(aArea)
	
	// Grava informações do recibo da tabela SE1
	RecLock("SE1",.F.)
		SE1->E1_XRECSER := cSerRec
		SE1->E1_XRECNUM := cNrRec
	MsUnLock()
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! EXCLUIREC ! Autor !Gilson Lima 		 ! Data ! 28/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao de Exclusão do recibo na tabela SZR					 !
! 			!  																 !
+----------------------------------------------------------------------------*/
Static Function ExcluiRec()
	
	Local aArea 	:= GetArea()
	Local cChave	:= SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA
	
	// Muda Status do Recibo para 2 = Excluído
	dbSelectArea("SZR")
	SZR->(dbSetOrder(2))
	SZR->(dbSeek(xFilial("SZR")+cChave))
	
	While SZR->(!EOF()) .And. SZR->(ZR_TITPREF+ZR_TITNUM+ZR_TITPARC) == cChave
		
		RecLock("SZR",.F.)
			SZR->ZR_STATUS := '2'	// 2 = Excluído
		SZR->(MsUnlock())
		
		SZR->(dbSkip())
	End
	
	SZR->(dbCloseArea())	

	RestARea(aArea)
	
	// Retira informações do recibo da tabela SE1
	RecLock("SE1",.F.)
		SE1->E1_XRECSER := ''
		SE1->E1_XRECNUM := ''
	MsUnLock()
	
	MsgInfo("Recibo Excluído com Sucesso!","Atenção")
	
Return