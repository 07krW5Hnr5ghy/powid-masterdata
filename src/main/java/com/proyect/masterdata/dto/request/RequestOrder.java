package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Customer;
import com.proyect.masterdata.domain.Item;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrder {

    private String deliveryMan;

    private String deliveryManPhone;

    private String salesman;

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

    private Item[] items;

    private Customer customer;
}
