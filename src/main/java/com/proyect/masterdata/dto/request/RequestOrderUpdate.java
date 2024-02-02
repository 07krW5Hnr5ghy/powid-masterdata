package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderUpdate {
    public String paymentMethod;
    public String saleChannel;
    public String observations;
    public String orderState;
    public String paymentState;
}
