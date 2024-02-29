package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestMembershipPayment {
    private Double netAmount;
    private Double grossAmount;
    private Double paymentGatewayFee;
    private Double taxAmount;
    private String subscriptionName;
    private Boolean demo;
    private List<String> modules;
    private String paymentGateway;
}
