package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class MembershipPaymentDTO {
    private UUID id;
    private String user;
    private Double grossAmount;
    private Double netAmount;
    private Double paymentGatewayFee;
    private Double taxAmount;
    private String paymentGateway;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
