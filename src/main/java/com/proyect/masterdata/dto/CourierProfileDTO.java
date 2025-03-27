package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CourierProfileDTO {
    private Long deliveredOrders;
    private Double payableAmount;
    private CourierDTO courierInfo;
}
