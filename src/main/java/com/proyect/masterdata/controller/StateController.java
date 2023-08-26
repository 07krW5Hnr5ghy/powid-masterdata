package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IState;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/state")
@AllArgsConstructor
public class StateController {

    private final IState iState;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iState.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/states")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iState.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<StateDTO> update(
            @RequestBody() RequestState requestState
    ) throws BadRequestExceptions {
        StateDTO result = iState.update(requestState);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iState.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("/list")
    public ResponseEntity<List<StateDTO>> listState() throws BadRequestExceptions {
        List<StateDTO> result = iState.listState();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<StateDTO>> listStatusFalse() throws BadRequestExceptions {
        List<StateDTO> result = iState.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<StateDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        StateDTO result = iState.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
