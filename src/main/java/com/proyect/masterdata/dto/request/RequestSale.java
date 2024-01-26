package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSale {

    private String seller;

    private String observations;

    private String paymentReceipt;

    private String deliveryAddress;

    private Double saleAmount;

    private Double deliveryAmount;

    private Double advancedPayment;

    private String paymentState;

    private String saleChannel;

    private String paymentMethod;

    private String managementType;
}
