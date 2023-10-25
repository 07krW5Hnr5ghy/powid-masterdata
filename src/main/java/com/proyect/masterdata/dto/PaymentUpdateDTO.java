package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PaymentUpdateDTO {
    private double totalPayment;
    private double discount;
    private String urlInvoice;
    private String month;
    private String paymentState;
    private String channel;
}
