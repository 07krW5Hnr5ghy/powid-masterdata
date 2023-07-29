package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.repository.StateRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class StateImpl implements IMasterList {
    private final StateRepository stateRepository;
    private final StateMapper stateMapper;
    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return stateMapper.stateListToStateListDTO(stateRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            stateRepository.save(State.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            State record = stateRepository.findById(id).get();
            stateRepository.save(State.builder().name(record.getName()).dateRegistration(new Date(System.currentTimeMillis())).id(record.getId()).status(false).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            State state = stateRepository.save(State.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build());
            return StateMapper.INSTANCE.stateToStateDTO(state);
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }


}
