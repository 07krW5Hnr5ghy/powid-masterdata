package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentMethod;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentMethod;
import io.swagger.v3.oas.annotations.Operation;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-method")
@AllArgsConstructor
public class PaymentMethodController {

    private final IPaymentMethod iPaymentMethod;

    @Operation(summary = "lista los metodos de pago ",
            description = "Lista los metodos de pago maestro")
    @GetMapping()
    public ResponseEntity<List<PaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        List<PaymentMethodDTO>  result = iPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registrar los metodos de pago ",
            description = "Registrar los metodos de pago")
    @PostMapping()
    public ResponseEntity<ResponsePaymentMethod> addPaymentMethod(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponsePaymentMethod result = iPaymentMethod.addPaymentMethod(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Borrar los metodos de pago ",
            description = "Borrar los metodos de pago")
    @DeleteMapping()
    public ResponseEntity<ResponsePaymentMethod> deletePaymentMethod(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponsePaymentMethod result = iPaymentMethod.deletePaymentMethod(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar los metodos de pago ",
            description = "Editar los metodos de pago")
    @PutMapping()
    public ResponseEntity<PaymentMethodDTO> updatePaymentMethod(@RequestBody PaymentMethodDTO data) throws BadRequestExceptions{
        PaymentMethodDTO result = iPaymentMethod.updatePaymentMethod(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
