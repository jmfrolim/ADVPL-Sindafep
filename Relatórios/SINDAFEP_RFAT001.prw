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
!Nome              ! RFAT001                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Relatório de autorização de débito do Banco do Brasil.  !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 18/12/2014                                              !
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

#DEFINE CRLF CHR(13) + CHR(10)

User Function RFAT001()
	Local oPrint := Nil
	Local oFont10
	Local oFont14
	Local oFont18
	Local oFont24
	Local nRow := 0
	Local cNomeCli := ""
	Local cCPFCli := ""
	Local cEndCli := ""
	Local cCidCli := ""
	Local cUFCli := ""
	Local cCEPCli := ""
	Local cAgencia := ""
	Local cNomeAg := ""
	Local aConta := {}
	Local cContaNum := ""
	Local cDAC := ""
	Local cOpera := ""
	Local lDados := .F.
	
	oFont10  := TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14  := TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont18  := TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont24  := TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	If MsgYesNo("Imprimir dados do cadastro?")
		lDados := .T.
	EndIf
	
	oPrint:= TMSPrinter():New("Autorizacao Debito BB")
	oPrint:Setup()
	oPrint:SetPortrait()
	oPrint:StartPage()
	
	If lDados	// Imprime ou não dados do cadastro
		If(SA1->A1_XBCOUSR == "N")
			cNomeCli := AllTrim(SA1->A1_XNOME)
			cCPFCli := AllTrim(SA1->A1_XCTACPF)
		Else
			cNomeCli := AllTrim(SA1->A1_NOME)
			cCPFCli := AllTrim(SA1->A1_CGC)
		EndIf
		
		cEndCli := AllTrim(SA1->A1_END)
		cCidCli := AllTrim(SA1->A1_MUN)
		cUFCli := AllTrim(SA1->A1_EST)
		cCEPCli := AllTrim(SA1->A1_CEP)
		
		cAgencia := AllTrim(SA1->A1_XAGENCI)
		// cNomeAg := AllTrim(SA1->A1_XAGENCI)
		aConta := StrTokArr(SA1->A1_XCONTA, '-')
		cContaNum := AllTrim(aConta[1])
		If Len(aConta) > 1
			cDAC := AllTrim(aConta[2])
		EndIf
	EndIf
	
	nRow := 20
	
	oPrint:Say(nRow,200,"AUTORIZAÇÃO PARA DÉBITO EM CONTA CORRENTE",oFont24)
	
	oPrint:Say(nRow+200,0100,"1o Titular",oFont10)
	oPrint:Say(nRow+300,0105,cNomeCli,oFont14)
	
	oPrint:Say(nRow+200,1500,"C.P.F.",oFont10)
	oPrint:Say(nRow+300,1505,cCPFCli,oFont14)
	
	oPrint:Line (nRow+0250,0100,nRow+0350,0100)
	oPrint:Line (nRow+0350,0100,nRow+0350,2000)
	oPrint:Line (nRow+0250,1500,nRow+0350,1500)
	oPrint:Line (nRow+0250,2000,nRow+0350,2000)
	
	oPrint:Say(nRow+400,0100,"Endereço",oFont10)
	oPrint:Say(nRow+500,0105,cEndCli,oFont14)
	
	oPrint:Line (nRow+0450,0100,nRow+0550,0100)
	oPrint:Line (nRow+0550,0100,nRow+0550,2000)
	oPrint:Line (nRow+0450,2000,nRow+0550,2000)
	
	oPrint:Say(nRow+600,0100,"Cidade",oFont10)
	oPrint:Say(nRow+700,0105,cCidCli,oFont14)
	
	oPrint:Say(nRow+600,1100,"U.F.",oFont10)
	oPrint:Say(nRow+700,1105,cUFCli,oFont14)
	
	oPrint:Say(nRow+600,1300,"C.E.P.",oFont10)
	oPrint:Say(nRow+700,1305,cCEPCli,oFont14)
	
	oPrint:Line (nRow+0650,0100,nRow+0750,0100)
	oPrint:Line (nRow+0750,0100,nRow+0750,2000)
	oPrint:Line (nRow+0650,1100,nRow+0750,1100)
	oPrint:Line (nRow+0650,1300,nRow+0750,1300)
	oPrint:Line (nRow+0650,2000,nRow+0750,2000)
	
	// Segundo Titular.
	
	oPrint:Say(nRow+0800,0100,"2o Titular",oFont10)
	// oPrint:Say(nRow+0900,0105,cNomeCli,oFont14)
	
	oPrint:Say(nRow+0800,1500,"C.P.F.",oFont10)
	// oPrint:Say(nRow+0900,1505,cCPFCli,oFont14)
	
	oPrint:Line (nRow+0850,0100,nRow+0950,0100)
	oPrint:Line (nRow+0950,0100,nRow+0950,2000)
	oPrint:Line (nRow+0850,1500,nRow+0950,1500)
	oPrint:Line (nRow+0850,2000,nRow+0950,2000)
	
	oPrint:Say(nRow+1000,0100,"Endereço",oFont10)
	// oPrint:Say(nRow+1100,0105,cEndCli,oFont14)
	
	oPrint:Line (nRow+1050,0100,nRow+1150,0100)
	oPrint:Line (nRow+1150,0100,nRow+1150,2000)
	oPrint:Line (nRow+1050,2000,nRow+1150,2000)
	
	oPrint:Say(nRow+1200,0100,"Cidade",oFont10)
	// oPrint:Say(nRow+1300,0105,cCidCli,oFont14)
	
	oPrint:Say(nRow+1200,1100,"U.F.",oFont10)
	// oPrint:Say(nRow+1300,1105,cUFCli,oFont14)
	
	oPrint:Say(nRow+1200,1300,"C.E.P.",oFont10)
	// oPrint:Say(nRow+1300,1305,cCEPCli,oFont14)
	
	oPrint:Line (nRow+1250,0100,nRow+1350,0100)
	oPrint:Line (nRow+1350,0100,nRow+1350,2000)
	oPrint:Line (nRow+1250,1100,nRow+1350,1100)
	oPrint:Line (nRow+1250,1300,nRow+1350,1300)
	oPrint:Line (nRow+1250,2000,nRow+1350,2000)
	
	oPrint:Say(nRow+1400,100,"CONTA CORRENTE",oFont18)
	
	oPrint:Say(nRow+1500,100,"Agência",oFont10)
	oPrint:Say(nRow+1600,105,cAgencia,oFont14)
	oPrint:Say(nRow+1500,350,"Nome Agência / Contato Gerência / Número Telefone",oFont10)
	oPrint:Say(nRow+1600,305,cNomeAg,oFont14)
	oPrint:Say(nRow+1500,1100,"Conta Número",oFont10)
	oPrint:Say(nRow+1600,1105,cContaNum,oFont14)
	oPrint:Say(nRow+1500,1650,"DAC",oFont10)
	oPrint:Say(nRow+1600,1655,cDAC,oFont14)
	
	oPrint:Line (nRow+1550,0100,nRow+1650,0100)
	oPrint:Line (nRow+1650,0100,nRow+1650,2000)
	oPrint:Line (nRow+1550,0350,nRow+1650,0350)
	oPrint:Line (nRow+1550,1100,nRow+1650,1100)
	oPrint:Line (nRow+1550,1650,nRow+1650,1650)
	oPrint:Line (nRow+1550,2000,nRow+1650,2000)
	
	oPrint:Say(nRow+1750,200,"Autorizamos que sejam debitados na conta do BANCO DO BRASIL S/A, acima",oFont14)
	oPrint:Say(nRow+1800,100,"indicada, todos os valores relativos as dívidas contratadas junto ao SINDAFEP",oFont14)
	oPrint:Say(nRow+1850,100,"- Sindicato dos Auditores Fiscais da Receita do Estado do Paraná, abaixo indicadas:",oFont14)
	
	oPrint:Say(nRow+2150,200,"Por força desta autorização, comprometemo-nos a manter saldo suficiente na",oFont14)
	oPrint:Say(nRow+2200,100,"conta corrente para acolher os referidos débitos.",oFont14)
	
	oPrint:Say(nRow+2350,100,Replicate("_", 20) + ", " + Replicate("_", 7) + " de " + Replicate("_", 20) + " de " + Replicate("_", 10),oFont14)
	
	oPrint:Say(nRow+2550,100,Replicate("_", 60),oFont14)
	oPrint:Say(nRow+2600,100,"1o Titular",oFont14)
	
	oPrint:Say(nRow+2800,100,Replicate("_", 60),oFont14)
	oPrint:Say(nRow+2850,100,"2o Titular",oFont14)
	
	oPrint:EndPage()
	oPrint:Preview()
Return
