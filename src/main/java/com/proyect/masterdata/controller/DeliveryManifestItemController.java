package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CourierProfileDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("delivery-manifest-item")
@AllArgsConstructor
public class DeliveryManifestItemController {
    private final IDeliveryManifestItem iDeliveryManifestItem;
    @PutMapping("/delivered/{deliveryManifestItemId}")
    public ResponseEntity<ResponseSuccess> markDeliveredItem(
            @PathVariable UUID deliveryManifestItemId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifestItem.markDeliveredDeliveryManifestItem(deliveryManifestItemId,user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("/collected/{deliveryManifestItemId}")
    public ResponseEntity<ResponseSuccess> markCollectedItem(
            @PathVariable UUID deliveryManifestItemId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifestItem.markDeliveredDeliveryManifestItem(deliveryManifestItemId,user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<Page<DeliveryManifestItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "quantity",required = false) Integer quantity,
            @RequestParam(value = "collected",required = false) Boolean collected,
            @RequestParam(value = "orderNumber",required = false) Long orderNumber,
            @RequestParam(value = "manifestNumber",required = false) Long manifestNumber,
            @RequestParam(value = "color",required = false) String color,
            @RequestParam(value = "size",required = false) String size,
            @RequestParam(value = "model",required = false) String model,
            @RequestParam(value = "brand",required = false) String brand,
            @RequestParam(value = "courier",required = false) String courier,
            @RequestParam(value = "courierDni",required = false) String courierDni,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize,
            @RequestParam(value = "delivered",required = false) Boolean delivered
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DeliveryManifestItemDTO>> result = iDeliveryManifestItem.list(
                user,
                quantity,
                collected,
                orderNumber,
                manifestNumber,
                color,
                size,
                model,
                brand,
                delivered,
                courier,
                courierDni,
                warehouse,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("profile")
    public ResponseEntity<CourierProfileDTO> courierProfile(
            @RequestParam(value = "registrationStartDate") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "user") String username
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<CourierProfileDTO> result = iDeliveryManifestItem.courierProfile(registrationStartDate,registrationEndDate,username);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
