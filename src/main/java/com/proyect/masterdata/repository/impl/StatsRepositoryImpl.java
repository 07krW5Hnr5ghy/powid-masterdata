package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.repository.StatsRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public class StatsRepositoryImpl implements StatsRepository {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public List<CategoryProduct> findAllCategoryProducts() {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<CategoryProduct> cq = cb.createQuery(CategoryProduct.class);
        Root<CategoryProduct> root = cq.from(CategoryProduct.class);
        cq.select(root);
        return entityManager.createQuery(cq).getResultList();
    }

    @Override
    public List<SaleChannel> findAllSaleChannels() {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<SaleChannel> cq = cb.createQuery(SaleChannel.class);
        Root<SaleChannel> root = cq.from(SaleChannel.class);
        cq.select(root);
        return entityManager.createQuery(cq).getResultList();
    }

    @Override
    public List<Ordering> findOrdersByClientAndRegistrationDate(Long clientId, Date startDate, Date endDate) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<Ordering> cq = cb.createQuery(Ordering.class);
        Root<Ordering> root = cq.from(Ordering.class);
        Predicate clientPredicate = cb.equal(root.get("clientId"), clientId);
        Predicate datePredicate = cb.between(root.get("registrationDate"), startDate, endDate);
        cq.select(root).where(cb.and(clientPredicate, datePredicate));
        return entityManager.createQuery(cq).getResultList();
    }

    @Override
    public List<OrderItem> findOrderItemsByClientAndOrder(Long clientId, Long orderId) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderItem> cq = cb.createQuery(OrderItem.class);
        Root<OrderItem> root = cq.from(OrderItem.class);
        Predicate clientPredicate = cb.equal(root.get("clientId"), clientId);
        Predicate orderPredicate = cb.equal(root.get("orderId"), orderId);
        Predicate statusPredicate = cb.isTrue(root.get("status"));
        cq.select(root).where(cb.and(clientPredicate, orderPredicate, statusPredicate));
        return entityManager.createQuery(cq).getResultList();
    }
}
