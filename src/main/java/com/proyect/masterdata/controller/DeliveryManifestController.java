package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DeliveryManifestCourierDTO;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifest;
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
@RequestMapping("delivery-manifest")
@AllArgsConstructor
public class DeliveryManifestController {
    private final IDeliveryManifest iDeliveryManifest;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestDeliveryManifest requestDeliveryManifest
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        //System.out.println(requestDeliveryManifest);
        CompletableFuture<ResponseSuccess> result = iDeliveryManifest.save(requestDeliveryManifest);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("/{deliveryManifestId}")
    public ResponseEntity<DeliveryManifestDTO> getManifestById(
            @PathVariable UUID deliveryManifestId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<DeliveryManifestDTO> result = iDeliveryManifest.getById(deliveryManifestId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping("/add/{deliveryManifestId}")
    public ResponseEntity<ResponseSuccess> addOrderDeliveryManifest(
            @PathVariable UUID deliveryManifestId,
            @RequestBody() RequestDeliveryManifest requestDeliveryManifest
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifest.addOrderDeliveryManifest(requestDeliveryManifest,deliveryManifestId, 1L);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("/check/{courierId}")
    public ResponseEntity<DeliveryManifestCourierDTO> checkCourierToDeliveryManifest(
            @PathVariable UUID courierId
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<DeliveryManifestCourierDTO> result = iDeliveryManifest.checkCourierToDeliveryManifest(courierId);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping("/close")
    public ResponseEntity<ResponseSuccess> closeManifest(
            @RequestParam("deliveryManifestId") UUID deliveryManifestId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifest.closeDeliveryManifest(deliveryManifestId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<Page<DeliveryManifestDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "manifestNumber",required = false) Long manifestNumber,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "courier",required = false) String courier,
            @RequestParam(value = "courierDni",required = false) String courierDni,
            @RequestParam(value = "courierUser",required = false) String courierUser,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize,
            @RequestParam(value = "open", required = false) Boolean open
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DeliveryManifestDTO>> result = iDeliveryManifest.list(
                user,
                manifestNumber,
                warehouse,
                courier,
                courierDni,
                courierUser,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                open
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("last")
    public ResponseEntity<DeliveryManifestDTO> getLast(
            @RequestParam(value = "user", required = true) String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<DeliveryManifestDTO> result = iDeliveryManifest.getLastDeliveryManifestByCourier(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }


}
