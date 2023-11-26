package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.Payment;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentSave;
import com.proyect.masterdata.dto.request.RequestPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPayment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/payment")
@AllArgsConstructor
public class PaymentController {

    private final IPayment iPayment;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("channel") String channel,
            @RequestBody() RequestPaymentSave requestPaymentSave,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iPayment.save(channel, requestPaymentSave, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payments", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("channel") String channel,
            @RequestBody() List<RequestPaymentSave> requestPaymentSaveList,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iPayment.saveAll(channel, requestPaymentSaveList, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PaymentUpdateDTO> update(
            @RequestBody() RequestPaymentUpdate requestPaymentUpdate,
            @RequestParam("newPaymentState") String newPaymentState,
            @RequestParam("user") String user) throws BadRequestExceptions {
        PaymentUpdateDTO result = iPayment.update(requestPaymentUpdate, newPaymentState, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<Payment>> list(
            @RequestParam(value = "totalPayment", required = false) Double totalPayment,
            @RequestParam(value = "month", required = false) String month,
            @RequestParam(value = "channel", required = false) String channel,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<Payment> result = iPayment.list(totalPayment, month, channel, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
