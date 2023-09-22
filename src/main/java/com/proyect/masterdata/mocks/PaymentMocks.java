package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.PaymentDTO;

import java.sql.Date;
import java.util.Collections;
import java.util.List;

public class PaymentMocks {
    List<PaymentDTO> payment1 = Collections.singletonList(PaymentDTO.builder()
            .dni("123456789")
            .name("alejandro")
            .surname("gomez")
            .email("agomes@gmail.com")
            .phoneNumber("123456789")
            .paymentStatusName("aceptado")
            .ecommerceName("eshop ltda")
            .billUrl("http://example.com")
            .paymentMethodName("tarjeta visa")
            .totalPaymentPerMonth(45.20)
            .startDate(new Date(System.currentTimeMillis()))
            .paymentDate(new Date(System.currentTimeMillis()))
            .monthsPayed(3)
            .build());


}
