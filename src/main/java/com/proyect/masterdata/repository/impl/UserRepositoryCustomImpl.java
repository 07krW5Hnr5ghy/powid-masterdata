package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.UserRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class UserRepositoryCustomImpl implements UserRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<User> searchForUser(
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Long status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<User> criteriaQuery = criteriaBuilder.createQuery(User.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(user,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> userList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                userList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                userList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(userList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<User> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(user,status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            String username,
            Long status,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();
        if(username!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("user")),username.toUpperCase()
                            )
                    )
            );
        }
        if(status!=null){
            conditions.add(criteriaBuilder.equal(itemRoot.get("status"),status));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot
    ){
        List<Order> userList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("USER")){
            userList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return userList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot
    ){
        List<Order> userList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("USER")){
            userList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return userList;
    }

    private long getOrderCount(String username,Long status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(username,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
