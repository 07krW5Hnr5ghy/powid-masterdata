package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ILogEvent;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/log-event")
@AllArgsConstructor
public class LogEventController {
    private final ILogEvent iLogEvent;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ResponseSuccess result = iLogEvent.save(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/log-events")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iLogEvent.saveAll(names);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<LogEventDTO> update(
            @RequestBody() RequestLogEvent requestLogEvent
    ) throws BadRequestExceptions {
        LogEventDTO result = iLogEvent.update(requestLogEvent);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iLogEvent.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/log-events")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iLogEvent.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<LogEventDTO>> list() throws BadRequestExceptions {
        List<LogEventDTO> result = iLogEvent.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<LogEventDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        LogEventDTO result = iLogEvent.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<LogEventDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        LogEventDTO result = iLogEvent.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
