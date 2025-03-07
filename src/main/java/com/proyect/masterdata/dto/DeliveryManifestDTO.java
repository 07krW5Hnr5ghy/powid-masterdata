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
    private List<DeliveryManifestItemDTO> deliveryManifestItemDTOS;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String courier;
    private Boolean open;
    private String warehouse;
    private String pickupAddress;
}
