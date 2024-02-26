package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestMembershipPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IMembershipPayment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("payment")
@AllArgsConstructor
public class MembershipPaymentController {

    private final IMembershipPayment iMembershipPayment;

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PaymentUpdateDTO> update(
            @RequestParam("membershipId") Long membershipId,
            @RequestBody() RequestMembershipPaymentUpdate requestMembershipPaymentUpdate,
            @RequestParam("user") String tokenUser) throws BadRequestExceptions {
        PaymentUpdateDTO result = iMembershipPayment.update(membershipId, requestMembershipPaymentUpdate, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    // @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    // public ResponseEntity<Page<MembershipPayment>> list(
    // @RequestParam(value = "totalPayment", required = false) Double totalPayment,
    // @RequestParam(value = "month", required = false) String month,
    // @RequestParam(value = "channel", required = false) String channel,
    // @RequestParam(value = "sort", required = false) String sort,
    // @RequestParam(value = "sortColumn", required = false) String sortColumn,
    // @RequestParam("pageNumber") Integer pageNumber,
    // @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
    // Page<MembershipPayment> result = iPayment.list(totalPayment, month, channel,
    // sort, sortColumn, pageNumber,
    // pageSize);
    // return new ResponseEntity<>(result, HttpStatus.OK);
    // }

}
