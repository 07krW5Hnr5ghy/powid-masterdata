package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestDeliveryManifestOrder {
    UUID deliveryManifestId;
    UUID orderId;
    String username;
    Double receivedAmount;
    String observations;
    Boolean deliveryFeeCollected;
    String paymentMethod;
    String orderDeliveryStatus;
}
