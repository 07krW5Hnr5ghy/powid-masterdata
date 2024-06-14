package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MembershipPaymentDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IMembershipPayment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("membership-payment")
@AllArgsConstructor
public class MembershipPaymentController {
    private final IMembershipPayment iMembershipPayment;
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:MEMBERSHIP_PAYMENT_GET')")
    public ResponseEntity<Page<MembershipPaymentDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "grossAmount", required = false) Double grossAmount,
            @RequestParam(value = "netAmount", required = false) Double netAmount,
            @RequestParam(value = "paymentGatewayFee", required = false) Double paymentGatewayFee,
            @RequestParam(value = "taxAmount", required = false) Double taxAmount,
            @RequestParam(value = "paymentGateway", required = false) String paymentGateway,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<MembershipPaymentDTO>> result = iMembershipPayment.list(
                user,
                grossAmount,
                netAmount,
                paymentGatewayFee,
                taxAmount,
                paymentGateway,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
