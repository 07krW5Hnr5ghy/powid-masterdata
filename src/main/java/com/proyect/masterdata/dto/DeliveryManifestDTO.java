package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestDTO {
    private UUID id;
    private String user;
    private Long manifestNumber;
    private List<DeliveryManifestOrderDTO> deliveryManifestOrderDTOS;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String courier;
    private String courierPhone;
    private String courierPlate;
    private Boolean open;
    private String warehouse;
    private String pickupAddress;
    private Double amount;
    private Double paidAmount;
    private Double payableAmount;
    private String observations;
    private Double productValue;
}
