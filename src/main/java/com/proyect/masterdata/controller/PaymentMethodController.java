package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IMasterList;
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

    private final IMasterList iPaymentMethod;

    @Operation(summary = "Lista los metodos de pago ",
            description = "Lista los metodos de pago maestro")
    @GetMapping()
    public ResponseEntity<List<MasterListDTO>> listPaymentMethods() throws BadRequestExceptions {
        List<MasterListDTO> result = iPaymentMethod.listRecords();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registrar los metodos de pago ",
            description = "Registrar los metodos de pago")
    @PostMapping()
    public ResponseEntity<ResponseMasterList> addPaymentMethod(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponseMasterList result = iPaymentMethod.addRecord(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Borrar los metodos de pago ",
            description = "Borrar los metodos de pago")
    @DeleteMapping()
    public ResponseEntity<ResponseMasterList> deletePaymentMethod(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponseMasterList result = iPaymentMethod.deleteRecord(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar los metodos de pago ",
            description = "Editar los metodos de pago")
    @PutMapping()
    public ResponseEntity<MasterListDTO> updatePaymentMethod(@RequestBody MasterListDTO data) throws BadRequestExceptions{
        MasterListDTO result = iPaymentMethod.updateRecord(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
