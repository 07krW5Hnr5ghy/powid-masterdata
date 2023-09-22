package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.PaymentDTO;
import lombok.Getter;

import java.sql.Date;

public class PaymentMocks {
    private PaymentDTO payment1 = PaymentDTO.builder()
            .dni("123456789")
            .name("ALEJANDRO")
            .surname("GOMEZ")
            .email("agomes@gmail.com")
            .phoneNumber("123456789")
            .paymentStatusName("aceptado")
            .ecommerceName("eshop ltda")
            .billUrl("https://firebasestorage.googleapis.com/v0/b/ecommerce-d633f.appspot.com/o/F45FFC9E-4B3C-49B5-A5DA-99A975486917.jpeg?alt=media&token=162344ce-dde9-464e-8862-07aee41b26fe")
            .paymentMethodName("tarjeta credito visa")
            .totalPaymentPerMonth(45.20)
            .startDate(new Date(2023,6,7))
            .paymentDate(new Date(2023,9,13))
            .monthsPayed(3)
            .build();
    private PaymentDTO payment2 = PaymentDTO.builder()
            .dni("3344446789")
            .name("GERARDO")
            .surname("VILLANUEVA")
            .email("gv77@gmail.com")
            .phoneNumber("1234554664")
            .paymentStatusName("en proceso")
            .ecommerceName("fireshop pty")
            .billUrl("https://firebasestorage.googleapis.com/v0/b/ecommerce-d633f.appspot.com/o/F45FFC9E-4B3C-49B5-A5DA-99A975486917.jpeg?alt=media&token=162344ce-dde9-464e-8862-07aee41b26fe")
            .paymentMethodName("yape app")
            .totalPaymentPerMonth(112.34)
            .startDate(new Date(2023,7,7))
            .paymentDate(new Date(2023,9,13))
            .monthsPayed(2)
            .build();
    private PaymentDTO payment3 = PaymentDTO.builder()
            .dni("894849038")
            .name("MARCELA")
            .surname("FUENTES")
            .email("mf_20@gmail.com")
            .phoneNumber("725454789")
            .paymentStatusName("rechazado")
            .ecommerceName("rd enterprises sa")
            .billUrl("https://firebasestorage.googleapis.com/v0/b/ecommerce-d633f.appspot.com/o/F45FFC9E-4B3C-49B5-A5DA-99A975486917.jpeg?alt=media&token=162344ce-dde9-464e-8862-07aee41b26fe")
            .paymentMethodName("efectivo")
            .totalPaymentPerMonth(150.89)
            .startDate(new Date(2023,5,10))
            .paymentDate(new Date(2023,9,13))
            .monthsPayed(4)
            .build();

    private PaymentDTO payment4 = PaymentDTO.builder()
            .dni("894849038")
            .name("CLAUDIA")
            .surname("VELEZ")
            .email("CV33@gmail.com")
            .phoneNumber("79876163413")
            .paymentStatusName("en revision")
            .ecommerceName("rd enterprises sa")
            .billUrl("https://firebasestorage.googleapis.com/v0/b/ecommerce-d633f.appspot.com/o/F45FFC9E-4B3C-49B5-A5DA-99A975486917.jpeg?alt=media&token=162344ce-dde9-464e-8862-07aee41b26fe")
            .paymentMethodName("tarjeta credito american express")
            .totalPaymentPerMonth(30.23)
            .startDate(new Date(2023,8,20))
            .paymentDate(new Date(2023,9,13))
            .monthsPayed(1)
            .build();
    @Getter
    private PaymentDTO[] paymentListDTO = {payment1,payment2,payment3,payment4};
}
