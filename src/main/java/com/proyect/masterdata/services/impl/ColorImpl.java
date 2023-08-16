package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.request.RequestColorSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.ColorMapper;
import com.proyect.masterdata.repository.ColorRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IColor;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ColorImpl implements IColor {
    private final ColorRepository colorRepository;
    private final ColorMapper colorMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions {

        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            colorRepository.save(colorMapper.colorToName(name.toUpperCase(), user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions{

        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            List<RequestColorSave> colorSaves = names.stream().map(data -> RequestColorSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            colorRepository.saveAll(colorMapper.listColorToListName(colorSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ColorDTO update(RequestColor requestColor) throws BadRequestExceptions {
        try {
            requestColor.setName(requestColor.getName().toUpperCase());
            requestColor.setUser(requestColor.getUser().toUpperCase());
            Color updatedColor = colorMapper.requestColorToColor(requestColor);
            updatedColor.setDateRegistration(new Date(System.currentTimeMillis()));
            Color color = colorRepository.save(updatedColor);
            return colorMapper.colorToColorDTO(color);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions{

        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            colorRepository.deleteByIdAndUser(code,user);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                colorRepository.deleteByIdAndUser(data,user);
            });
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<ColorDTO> list() throws BadRequestExceptions{
        try {
            return colorMapper.listColorToListColorDTO(colorRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ColorDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return colorMapper.colorToColorDTO(colorRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ColorDTO findByName(String name) throws BadRequestExceptions{
        try {
            return colorMapper.colorToColorDTO(colorRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<ColorDTO> findByUser(String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        
        try {
            return colorMapper.listColorToListColorDTO(colorRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
