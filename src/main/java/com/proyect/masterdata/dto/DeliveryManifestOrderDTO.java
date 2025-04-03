package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;

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
    private String phone;
    private String district;
    private String province;
    private UUID orderId;
    private UUID deliveryManifestId;
    private String observations;
    private Double receivedAmount;
    private String orderPaymentState;
    private Boolean deliveryFeeCollected;
    private Boolean delivered;
}
