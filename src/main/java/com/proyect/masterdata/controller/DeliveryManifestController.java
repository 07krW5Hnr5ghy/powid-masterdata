package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifest;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
        CompletableFuture<ResponseSuccess> result = iDeliveryManifest.save(requestDeliveryManifest);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<DeliveryManifestDTO> getManifestById(
            @RequestParam("id") UUID deliveryManifestId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<DeliveryManifestDTO> result = iDeliveryManifest.getById(deliveryManifestId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
