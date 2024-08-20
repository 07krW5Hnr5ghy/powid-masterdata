package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class MembershipPaymentDTO {
    private Double grossAmount;
    private Double netAmount;
    private Double paymentGatewayFee;
    private Double taxAmount;
    private String paymentGateway;
    private Date registrationDate;
    private Date updateDate;
}
