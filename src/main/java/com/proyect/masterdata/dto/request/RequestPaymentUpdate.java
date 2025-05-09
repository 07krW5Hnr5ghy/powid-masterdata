package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestPaymentUpdate {
    private String channel;
    private String month;
    private String paymentState;
    private String newInvoiceUrl;
}
