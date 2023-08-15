package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestCreatePaymentState;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentState;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-state")
@AllArgsConstructor
public class PaymentStateController {

    private final IPaymentState iPaymentState;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-states")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<RequestCreatePaymentState> requestCreatePaymentStateList
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.saveAll(requestCreatePaymentStateList);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<PaymentStateDTO> update(
            @RequestBody() RequestPaymentState requestPaymentState
    ) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.update(requestPaymentState);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentState.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/payment-states")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentState.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<PaymentStateDTO>> list() throws BadRequestExceptions {
        List<PaymentStateDTO> result = iPaymentState.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<PaymentStateDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<PaymentStateDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
