package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestOrderDTO {
    private Long orderNumber;
    private List<DeliveryManifestItemDTO> deliveryManifestItemDTOList;
    private Double payableAmount;
    private String paymentMethod;
    private Double advancePayment;
    private String orderState;
    private String dni;
    private String customer;
    private String address;
}
