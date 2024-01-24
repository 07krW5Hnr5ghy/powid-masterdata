package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Order;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSale {

    private Order order;

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
