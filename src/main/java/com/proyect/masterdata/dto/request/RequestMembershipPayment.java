package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestMembershipPayment {
    private double netAmount;
    private double grossAmount;
    private Integer months;
    private String invoiceUrl;
}
